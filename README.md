# RUiua
Interact with and run R code from Uiua. See [RUiuaPlot](https://github.com/DJAntonBruckner/RUiuaPlot).

## Requirements 
R and Rscript in your path and the latest `--main` version of Uiua. Then copy the .r and .ua files to your working directory. The "git module" functionality of Uiua is not supported. Only tested on Linux.

## Usage
Import RUiua as a module using `R ~ "git: github.com/DJAntonBruckner/RUiua"`.
See [RUiuaPlot](https://github.com/DJAntonBruckner/RUiuaPlot) for an example of how to use RUiua module.

### RInterface
Allows interaction with R and executing R code. Also handles sending data to and retrieving data from R.

First, create a new R environment using `CreateEnv`. 
Then you can use `SourceFile` to run an R file,
`EvalString` to execute a string as R code and
`CallFunction` to call a function.
Only `CallFunction` can directly return values, so i suggest you create a R script with functions you want to call, source it to load them and then use `CallFunction` to call them.

Note that communication between R and Uiua is probably very slow and should be kept to a minimum. 

The function `Echo` is provided as a test for data transmission.

`CallFunction` is called using a boxed array of arguments and a boxed array of names for those arguments. Sometimes you may prefer to use a single (boxed) array of Name-Data pairs, you can use the `Pair` function and its inverse to convert between those representations (you may need to `:` the result). 

`AddDefaultArgument` can be used to add arguments to a existing array of name-data pairs, if the name does not already occur in the array.

Finally, call `StopR` to close R and all connections to it.

### RPlot
Uses RInterface to implement basic plots.

First, use `NewWindow` to create a new plot window, you then can plot stuff
using `SimplePlot`, `Plot` and `PlotFunction`. If you want multiple plots,
you can call `PartitionWindow` first. Use `WaitUntilWindowsClosed` to wait
until all plots are closed, then exit R using `StopR`.