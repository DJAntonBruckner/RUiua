# Plot using R

R ~ "RInterface.ua"

# ------INIT------
R~CreateEnv
Env ↚
◌R~SourceFile Env "RPlot.r"

# ------FUNCTIONS------ 

# Create a new window
NewWindow ← |0.0 (
    ◌R~CallFunction Env "new_window" [] []
)

# Plots the points `(x_1, y_1),(x_2,y_2)...`
# ? x y
SimplePlot ← |2.0 (
    ◌R~CallFunction Env "simple_plot" : {"x" "y"} ⊟∩□
)

# Plots using the values provided in `PlotArgs`.
# Use the format `{{Name Data}}` to provide them.
# The args are passed directly to the R function
# `plot`. See the documentation of the R function `plot`.
# ? PlotArgs
Plot ← |1.0 (
    ◌R~CallFunction Env "simple_plot" : °R~Pair
)

# Waits unitl all plot windows are closed
WaitUntilWindowsClosed ← |0.0 (
    ◌R~CallFunction Env "wait_until_windows_closed" [] []
)

# Generates `amount + 1` points evenly spaced in the interval
# `[min max]`
# points ? min max amount
PointsInInterval ← |3 +×⇡+1⊙÷.⊙::⊙:⊙-.

# Plots the Function in the interval `[min max]` using
# `amount + 1` points. 
# Provide `{}` as `PlotArgs` if you dont care about them.
# ? min max amount PlotArgs
PlotFunction! ← (|4.0
    ∵^! . PointsInInterval
    ⊙⊙(°R~Pair R~AddDefaultArguments {{"type" "l"}})
    {"y" "x"} ⊟∩□
    ⊙⊂⊂⊙:
    R~Pair
    Plot
)

# Partitions the window for multiple plots.
# ? shape
PartitionWindow ← |1.0 (
    ◌R~CallFunction Env "partition_window" : {"shape"} {∘}
)

StopR ← R~StopR
