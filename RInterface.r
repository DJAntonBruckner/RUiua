###### CONSTANTS ######

command_types <- list("ENV", "SOURCE", "EVAL", "CALL", "ECHO", "END")
response_types <- list("RESPONSE", "ERROR")
data_types <- list("int32", "string", "csv_matrix", "list")
max_message_size <- 2^31 - 1

###### VARS ######

env_list <- list()

###### FUNCTIONS ######

get_command_name <- function(id) {
    name <- NA_character_
    try(name <- command_types[[id + 1]], silent = TRUE)
    return(name)
}
get_response_id <- function(name) match(name, response_types) - 1
get_data_type_name <- function(id) {
    name <- NA_character_
    try(name <- data_types[[id + 1]], silent = TRUE)
    return(name)
}
get_data_type_id <- function(name) match(name, data_types) - 1

read_i32 <- function() {
    readBin(con, "integer",
        size = 4,
        endian = "little"
    )
}

read_string <- function() {
    size <- read_i32()

    if (is.na(size) || size < 0) {
        stop("Error while receiving string: invalid size")
    }

    iconv(
        readChar(con, size, useBytes = TRUE),
        from = "UTF-8"
    )
}

read_csv_matrix <- function() {
    size <- read_i32()

    if (is.na(size) || size < 0) {
        stop("Error while receiving matrix: invalid size")
    }
    if (size == 0) {
        return(matrix(, 0, 0))
    }

    unname(
        as.matrix(
            read.csv(
                text = readChar(con, size, useBytes = TRUE),
                header = FALSE,
                encoding = "UTF-8"
            )
        )
    )
}

send_i32 <- function(num) {
    writeBin(
        object = as.integer(num),
        con = con,
        size = 4,
        endian = "little"
    )
}

send_string <- function(msg) {
    size <- nchar(msg, type = "bytes")
    send_i32(size)

    if (size == 0) {
        return()
    }

    writeChar(
        msg,
        con,
        nchars = size,
        eos = NULL,
        useBytes = TRUE
    )
}

get_csv_matrix_string <- function(mat) {
    matstr <- ""
    tcon <- textConnection("matstr", "w", local = TRUE)
    write.table(mat, tcon, sep = ",", row.names = FALSE, col.names = FALSE)
    close(tcon)
    return(paste0(matstr, collapse = "\n"))
}

receive_list <- function() {
    list_length <- read_i32()

    if (is.na(list_length) || list_length < 0) {
        stop("Invalid Argument Length!")
    }

    arg_list <- list(data = list(), type = list())

    for (i in seq_len(list_length)) {
        arg_type <- get_data_type_name(read_i32())
        arg_list$type[[i]] <- arg_type

        if (arg_type == "int32") {
            arg_list$data[[i]] <- read_i32()
            next
        }

        if (arg_type == "string") {
            arg_list$data[[i]] <- read_string()
            next
        }

        if (arg_type == "csv_matrix") {
            arg_list$data[[i]] <- read_csv_matrix()
            next
        }

        if (arg_type == "list") {
            arg_list$data[[i]] <- receive_list()
            next
        }

        stop("Invalid argument type")
    }

    return(arg_list)
}

receive_request <- function() {
    command_name <- get_command_name(read_i32())
    command_args <- receive_list()
    arg_names <- receive_list()

    if (length(arg_names$data) != 0) {
        if (!all(arg_names$type == "string")) {
            stop("Received non string name")
        }

        names(command_args$data) <- c(NA, NA, arg_names$data)
    }

    return(list(command = command_name, args = command_args))
}

simplify_and_verify <- function(response) {
    if (class(response$data) != "list") {
        stop(paste0(
            "Error: Incorrect data type, is ",
            class(response), ", expected list"
        ))
    }

    if (length(response) >= max_message_size) {
        stop("Error: List is wayyyy to long")
    }

    if (length(response$data) != length(response$type)) {
        stop("Error: data and type list lengths must be equal")
    }

    simple_response <- list(data = list(), type = response$type)

    for (i in seq_along(response$data)) {
        resp_data <- response$data[[i]]
        resp_type <- response$type[[i]]

        if (resp_type == "int32") {
            simple_response$data[[i]] <-
                as.integer(resp_data)
            next
        }

        if (resp_type == "string") {
            str <- iconv(resp_data, to = "UTF-8")

            if (nchar(str, type = "bytes") >= max_message_size) {
                stop("Error: Response string to long!")
            }

            simple_response$data[[i]] <- str
            next
        }

        if (resp_type == "csv_matrix") {
            str <- iconv(get_csv_matrix_string(resp_data), to = "UTF-8")
            if (nchar(str, type = "bytes") >= max_message_size) {
                stop("Error: Converted csv to long!")
            }

            simple_response$data[[i]] <- str
            next
        }

        if (resp_type == "list") {
            simple_response$data[[i]] <-
                simplify_and_verify(resp_data)
            next
        }

        stop(paste("Error: Could not verify type", resp_type))
    }

    return(simple_response)
}

send_response_list <- function(response) {
    send_i32(length(response$data))

    for (i in seq_along(response$data)) {
        resp_data <- response$data[[i]]
        resp_type <- response$type[[i]]

        resp_type_id <- get_data_type_id(resp_type)

        if (is.na(resp_type_id)) {
            stop(paste("FATAL ERROR: could not resolve response type:", resp_type))
        }

        send_i32(resp_type_id)

        if (resp_type == "int32") {
            send_i32(resp_data)
            next
        }

        if (resp_type == "string" || resp_type == "csv_matrix") {
            send_string(resp_data)
            next
        }

        if (resp_type == "list") {
            send_response_list(resp_data)
            next
        }

        stop(paste("FATAL ERROR: unimplented response type (should never happen!)"))
    }
}

# generic send to uiua function
send_response <- function(response, error = FALSE) {
    had_error <- FALSE
    tryCatch(
        {
            clean_response <- simplify_and_verify(response)
        },
        error = function(e) {
            had_error <<- TRUE
            send_error(e)
        }
    )

    if (had_error) {
        return()
    }

    if (error) {
        send_i32(get_response_id("ERROR"))
    } else {
        send_i32(get_response_id("RESPONSE"))
    }

    send_response_list(clean_response)
}

# send the error_message to uiua as an error
send_error <- function(error_message) {
    send_response(
        list(
            data = list(error_message),
            type = list("string")
        ),
        error = TRUE
    )
}

# send a "OK" resonse to uiua
send_ok <- function() {
    send_response(list(data = list("OK"), type = list("string")))
}

# check if there are enough args and they have the correct type
expect_args <- function(arg, expected) {
    if (length(arg$data) < length(expected)) {
        send_error(paste(
            "Not enough args for command!",
            "is:", length(arg$data), "expected:", length(expected)
        ))
        return(FALSE)
    }
    for (i in seq_along(expected)) {
        arg_type <- arg$type[[i]]
        expected_type <- expected[[i]]

        if (arg_type != expected_type) {
            send_error(
                paste(
                    "Arg", i, "has wrong type,",
                    "is:", arg_type, "expected:", expected_type
                )
            )
            return(FALSE)
        }
    }
    return(TRUE)
}

# the "ENV" command
new_env <- function(arg) {
    expect_args(arg, c("string"))

    tag <- arg$data[[1]]

    if (hasName(env_list, tag)) {
        send_error("Error: ENV already present")
        return()
    }
    nenv <- new.env(parent = globalenv())
    assign("debug_stderr", debug_stderr, envir = nenv)
    assign("print_debug", print_debug, envir = nenv)
    env_list[[tag]] <<- nenv
    send_ok()
}

# the "EVAL" command
eval_str <- function(arg) {
    expect_args(arg, c("string", "string"))
    env_name <- arg$data[[1]]
    code <- arg$data[[2]]

    if (!hasName(env_list, env_name)) {
        send_error(paste("Unknown env:", env_name))
        return()
    }
    env <- env_list[[env_name]]

    had_error <- FALSE

    tryCatch(
        output <- capture.output(eval(
            expr = parse(
                text = code,
                keep.source = FALSE,
                encoding = "UTF-8"
            ),
            envir = env
        )),
        error = function(e) {
            send_error(e)
            had_error <<- TRUE
        }
    )

    if (had_error) {
        return()
    }

    send_response(list(
        data = list(paste0(output, collapse = "\n")),
        type = list("string")
    ))
}

# the "SOURCE" command
source_file <- function(arg) {
    expect_args(arg, c("string", "string"))
    env_name <- arg$data[[1]]
    file_path <- arg$data[[2]]

    if (!hasName(env_list, env_name)) {
        send_error(paste("Unknown env:", env_name))
        return()
    }
    env <- env_list[[env_name]]

    if (!file.exists(file_path)) {
        send_error(paste("File not found: ", file_path))
    }

    had_error <- FALSE

    tryCatch(
        output <- capture.output(
            sys.source(file_path, envir = env)
        ),
        error = function(e) {
            send_error(e)
            had_error <<- TRUE
        }
    )

    if (had_error) {
        return()
    }

    send_response(list(
        data = list(paste0(output, collapse = "\n")),
        type = list("string")
    ))
}

# the "CALL" command
call_function <- function(arg) {
    expect_args(arg, c("string", "string"))
    env_name <- arg$data[[1]]
    function_name <- arg$data[[2]]

    if (!hasName(env_list, env_name)) {
        send_error(paste("Unknown env:", env_name))
        return()
    }
    env <- env_list[[env_name]]
    had_error <- FALSE

    result <- NULL

    tryCatch(
        result <- do.call(function_name, arg$data[-1:-2], envir = env),
        error = function(e) {
            send_error(e)
            had_error <<- TRUE
        }
    )

    if (had_error) {
        return()
    }

    convert_and_send(result)
}

# convert a list into a sendable format
convert_list <- function(result_list) {
    resp_list <- list(data = list(), type = list())

    for (i in seq_along(result_list)) {
        elem <- result_list[[i]]
        if (is.numeric(elem)) {
            resp_list$data[[i]] <- elem
            resp_list$type[[i]] <- "csv_matrix"
            next
        }
        if (is.character(elem)) {
            resp_list$data[[i]] <- paste0(elem, collapse = "\n")
            resp_list$type[[i]] <- "string"
            next
        }
        if (is.list(elem)) {
            resp_list$data[[i]] <- convert_list(elem)
            resp_list$type[[i]] <- "list"
            next
        }
        stop("Cannot convert result for transmission.")
    }

    return(resp_list)
}

# convert the result into a sendable format and send it to uiua
convert_and_send <- function(result) {
    if (is.null(result) || any(is.na(result))) {
        send_ok()
        return()
    }

    if (!is.list(result)) {
        result <- list(result)
    }

    resp <- list()
    had_error <- FALSE

    tryCatch(
        resp <- convert_list(result),
        error = function(e) {
            send_error(e)
            had_error <<- TRUE
        }
    )

    if (had_error) {
        return()
    }

    send_response(resp)
}

process_command <- function(com) {
    command <- com$command
    com_args <- com$args

    if (command == "ECHO") {
        send_response(com_args)
        return()
    }

    if (command == "END") {
        send_ok()
        loop <<- FALSE
        return()
    }

    if (command == "ENV") {
        new_env(com_args)
        return()
    }

    if (command == "EVAL") {
        eval_str(com_args)
        return()
    }

    if (command == "SOURCE") {
        source_file(com_args)
        return()
    }

    if (command == "CALL") {
        call_function(com_args)
        return()
    }

    stop("not implemented")
}

###### CODE ######

stdin_c <- file("stdin", open = "rt")

# reat init values from uiua
port <- readLines(stdin_c, 1)
debug_stderr <- readLines(stdin_c, 1) == "DEBUG"

print_debug <- function(msg) {
    if (debug_stderr == TRUE) {
        write(capture.output(print(msg)), stderr())
    }
}

if (debug_stderr) {
    print_debug("DEBUG ON")
} else {
    write("DEBUG OFF", stderr())
}

# send uiua available types
writeLines(unlist(list(
    length(command_types),
    command_types,
    length(response_types),
    response_types,
    length(data_types),
    data_types
)))

# connect to uiua
con <- socketConnection(port = port, open = "r+b", blocking = TRUE)


# LOOP
loop <- TRUE

while (loop) {
    process_command(receive_request())
}

close(con)
