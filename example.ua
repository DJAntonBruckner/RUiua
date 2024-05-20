Plot ~ "RPlot.ua"

Plot~NewWindow
Plot~PartitionWindow [2 2]
Plot~SimplePlot ⇡10 ⁿ⇡10 e
Plot~Plot {{"x" +10⇡50} {"y" ₙ2 +10⇡50} {"main" "Im a Title!"}}
Plot~PlotFunction!∿ 0 τ 100 {{"col" "red"} {"ylab" "sin(x)"}}
Plot~PlotFunction!√ 0 50 100 {}
Plot~WaitUntilWindowsClosed
Plot~StopR
