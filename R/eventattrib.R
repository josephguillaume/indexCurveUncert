eventattrib <-
function (X, events, FUN = mean,...)
{
    stopifnot(inherits(X, "zoo"))
    xValues <- eventapply(X, events = events, FUN = FUN, ...)
    xValuesend <- eventapply(X, events = events, FUN = FUN, TIMING = "end") ##New
    xLengths <- eventapply(X, events = events, FUN = NROW, TIMING = "middle",
        by.column = FALSE)
    midTimeComponents <- as.POSIXlt(time(xLengths)) ## mid time
    startTimeComponents <- as.POSIXlt(time(xValues)) ## start time
    Monthstart = startTimeComponents$mon + 1  ## start time
    Duration = coredata(xLengths)
    DryPeriod = preInterEventDuration(events)
    return(list(
                events=events,
                xValues=xValues,
                xValuesend=xValuesend,
                xLengths=xLengths,
                midTimeComponents=midTimeComponents,
                startTimeComponents=startTimeComponents,
                Monthstart=Monthstart,
                Duration=Duration,
                DryPeriod=DryPeriod,
                ddd=data.frame(StartTime = time(xValues), EndTime = time(xValuesend), Value = coredata(xValues), #EndTime new
                  Yearmid = midTimeComponents$year + 1900, Monthstart = Monthstart,
                  Duration = Duration, DryPeriod = DryPeriod
                  )
                ))
}
