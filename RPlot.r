new_window <- function() {
    x11()
}

simple_plot <- function(x, y = NULL, xlab = "x", ylab = "y", main = "Plot", ...) {
    if (dev.cur() == 1) {
        stop("Please create a window first!")
    }

    plot(x, y, xlab = xlab, ylab = ylab, main = main, ...)
}


wait_until_windows_closed <- function() {
    while (dev.cur() != 1) {
        Sys.sleep(0.1)
    }
}

partition_window <- function(shape) {
    par(mfrow = shape)
}
