# Interact with and run code in R

# ------INIT------

⊂⊂ WorkingDir ThisFileDir "/RInterface.r"

⍥(&exit 1 &p "File not Found: RInterface.r") ¬&fe .

&runs {"Rscript" "--vanilla" ∘}
# handle to the R child process
RProc ↚

# get the port number from the address
GetPort ↚ |1 °□⊢⇌ ⊜□ ≠":" .

# convert number to int32 (hopefully)
ToBytes ↚ |1 (
    ⍤"Too large" < ⁿ31 2 .
    ⍤"Too small" > ¯ⁿ31 2 .
    ⍜⋯(⬚0↯4_8) ⟨◌|+ⁿ⊙2⟩<0,32
)

# create int32 from bytes (hopefully)
FromBytes ↚ |1 ⍥(-ⁿ32 2)≥ ⁿ31 2 . ⍜⋯♭

# read line from R
ReadLine ↚ |0 (
    ↘¯1 &ru "\n" RProc
    ▽≠, @\r
)

# init connection to R
&tcpl "127.0.0.1:0"
GetPort &tcpaddr .

# send port and Debug flag
&w $"_\n" : RProc
&w "DEBUG\n" RProc

&tcpa
# the connection to R
RConn ↚

ReadInitList ↚ (⇌{⍥ReadLine} ⋕ReadLine)

# read the available responses, commands, and data types from r
Commands ↚ ReadInitList
Responses ↚ ReadInitList
DataTypes ↚ ReadInitList

# ------PRIVATE------

# creates a csv of complex numbers
ComplexCsv ↚ |1 setinv(
    ⍤"Error: must be array of numbers" /↥ =[1 2] type .
    ⍤"Error: dim must be 2 or 1" ≤ 2 ⧻△ .
    ∵(□$"__i"∩(⟨$"_"|$"+_"|$"+_"⟩+1±.):°ℂ)
    /$"_\n_"≡(□/$"_,_")
)(
    ⊜□≠,"\n"
    ≡(
        ⊜□≠,","°□
        ≡(
            ℂ:⍣(∩⋕°$"_+_i"|¯∩⋕°$"_-_i")
        )
    )
)

ProcessArgN ↚ |1 (
    type °□ .
    ⟨
        {"csv_matrix" csv}
    | &w "Warning: Complex numbers can be sent, but not received!\n" 2
        {"csv_matrix" ComplexCsv}
    | {"string" ♭}
    | {"list" ⍚ProcessArgN}
    ⟩
)
ProcessArgI ↚ |1 (
    ⊙°□°⊟
    ⊗:{"csv_matrix" "string" "list" "int32"}
    ⟨
        ⍥⍣°¤ ∘ 2 ∵⍣⋕ ∘ °csv
    | ∘
    | ⍚ProcessArgI
    | FromBytes
    | ⍤"Unhandeled type" 0
    ⟩
)

# setinv with recursive functions is bugged!
# Processes args so they can be sent to R or
# °processes args from R.
# use with ⍚!
ProcessArg ↚ setinvProcessArgN ProcessArgI

# send a int32 to R 
SendInt ↚ |1.0 &w ToBytes : RConn

# send a (pre-proccesed!) list to R
SendList ↚ |1.0 (
    SendInt ⧻ .
    ⍚(
        °⊟
        SendInt ⊗ : DataTypes .
        ≍□"list"
        ⟨
            utf °□
            SendInt ⧻.
            &w : RConn
          | SendList °□
        ⟩
    )
)

# read a int32 from R
ReadInt ↚ (FromBytes &rb 4 RConn)

# needs to be a modifier because its mutually recursive with
# ReadList. Only ever intended to be used on ReadList
ReadArg! ↚ (|0
    .⊡ ReadInt DataTypes
    ⊗:{"int32" "list"}
    ⟨
        ReadInt
    | ^!
    | ReadInt
        ⟨""◌|°utf&rb : RConn⟩ >0 .
    ⟩
    ⊟:□
)

# read a list from r
ReadList ↚ |0 (
    ReadInt
    ⇌{⍥(ReadArg!ReadList)}
)

AllTrue! ↚^ $"/×[⊃(_)]" /$"_|_"
OneTrue! ↚^ $"/↥[⊃(_)]" /$"_|_"

# is the argument either an empty array, a array of strings
# or a boxed array of strings
# bool ? arg
IsListOfStrings ↚ |1 OneTrue!(
    ≍[0] △
| AllTrue!(
        ≍2 type
    | OneTrue!(≍1 ⧻△|≍2 ⧻△)
    )
| AllTrue!(
        ≍3 type
    | °□/× ⍚(AllTrue!(≍2 type|≍1 ⧻△))
    )
)

# ? command args names
SendRCommand ↚ |3.0 (
    ⍤"Command must be a string!" ≍2 type .
    ⍤"Command must be flat!" ≍1 ⧻△ .
    ⊗□:Commands
    ⍤"Command not found!" ¬≍ ⧻Commands .
    ⊙⍚ProcessArg
    ⊙⊙⟜(⍤"NamesListInvalid" IsListOfStrings)

    SendInt
    SendList

    ⍥{∘} AllTrue!(≍2 type|≍1 ⧻△|> 0 ⧻) .
    ⍚ProcessArg
    SendList
)

ReceiveRResponse ↚ (
    ReadInt
    ReadList
    ⍚°ProcessArg
    ⊙⍣(⊡:Responses)(⍤"Unknown response type" 0 ∘)
    ⊟:□
)

# Returns the value of a normal response or
# throws an error if the response is an error
# value ? response
ResponseOrError ↚ (
    °⊟
    ≍□"RESPONSE"
    ⟨⍤:0.⊢°□|°□⟩
)

# ------PUBLIC------ 

# Creates a new environment in R where you can put
# your variables, source files, etc.
# Returns a string containing a unique tag for your env.
# A function `print_debug()` is provided in the env to
# print to stderr, since stdout is redirected to uiua
# EnvTag ?
CreateEnv ← |0 (
    SendRCommand "ENV" : [] {∘} .°⋕ tag
    ReceiveRResponse
    ◌ResponseOrError
)

# Uses the R function "source()" in the provided env.
# This essentially just runs an R file.
# The output is captured using "capture.output()" and returned.
# Output ? EnvTag FilePath
SourceFile ← |2 (
    SendRCommand "SOURCE" : [] ⊟∩□
    ReceiveRResponse
    ResponseOrError
)

# Evaluates a string using "eval(parse())" in the provided
# environment. The output is captured using "capture.output"
# and returned. If you need to return a value, not the captured output,
# please see CallFunction.
# Output ? EnvTag String
EvalString ← |2 (
    SendRCommand "EVAL" : [] ⊟∩□
    ReceiveRResponse
    ResponseOrError
)

# Calls the function named `FunctionName` in the specfied Environment,
# passing it the arguments in `FunctionArgs` (please use a boxed array).
# If `ArgNames` is not an empty array, the FunctionArgs are named before being
# passed to the function. Please note the `Pair` function (and its inverse) to convert between
# `{Name} {Args}` and `{{Name Arg}}` styles. Also note the `DefaultArgument` function.
# Output ? EnvTag FunctionName FunctionArgs ArgNames
CallFunction ← |4 (
    SendRCommand "CALL" ⊂⊟∩□
    ReceiveRResponse
    ResponseOrError
)

# Kills the R process and closes all connections to it
StopR ← |0.0 (
    SendRCommand "END" [] []
    ReceiveRResponse
    ◌ResponseOrError
    &cl RConn
    &cl RProc
)

# Test function. Sends data to R and immediately retrieves it.
# ECHO (echo echo echo)
# Data ? Data
Echo ← |1 (
    SendRCommand "ECHO" : []
    ReceiveRResponse
    ResponseOrError
)

# you can ° this!
# `{{x1 y1} {x2 y2} ...} ← {x1 x2 ...} {y1 y2 ...}`
Pair ← |2 ≡(□⊟)

# `? {{DefaultArgName DefaultArgData}} {{FunctionArgName FunctionArgData}}`
AddDefaultArguments ← |2 (
    ≍0 ⧻ .:
    ⟨
        ⍜∩°Pair (⊙(⊂▽):⊙(⊂▽).¬∊ ,,⊙:) :
    | ◌
    ⟩
)
