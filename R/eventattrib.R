eventattrib <-
function (X, events, FUN = mean,...)
{
  ## If no events, still return same structure, but with NULL values
  if(all(is.na(coredata(events))))
    return(list(
                events=events,
                xValues=NULL,
                xValuesend=NULL,
                xLengths=NULL,
                midTimeComponents=NULL,
                startTimeComponents=NULL,
                timing=NULL,
                duration=NULL,
                dry=NULL,
                ddd=list(StartTime = NULL,
                  EndTime = NULL,
                  Value = NULL,
                  Yearmid = NULL,
                  timing = NULL,
                  duration = NULL,
                  dry = NULL
                  )
                ))
  
  stopifnot(inherits(X, "zoo"))
  xValues <- eventapply(X, events = events, FUN = FUN, ...)
  xValuesend <- eventapply(X, events = events, FUN = FUN, TIMING = "end") ##New
  xLengths <- eventapply(X, events = events, FUN = NROW, TIMING = "middle",
                         by.column = FALSE)
  midTimeComponents <- as.POSIXlt(time(xLengths))  ## mid time
  startTimeComponents <- as.POSIXlt(time(xValues)) ## start time
  Monthstart = startTimeComponents$mon + 1         ## start time
  Duration = coredata(xLengths)
  DryPeriod = preInterEventDuration(events)
  ## TODO: simplify return value
  return(list(
              events=events,
              xValues=xValues,
              xValuesend=xValuesend,
              xLengths=xLengths,
              midTimeComponents=midTimeComponents,
              startTimeComponents=startTimeComponents,
              timing=Monthstart,
              duration=Duration,
              dry=DryPeriod,
              ddd=list(StartTime = time(xValues),
                EndTime = time(xValuesend),
                Value = coredata(xValues), #EndTime new
                Yearmid = midTimeComponents$year + 1900,
                timing = Monthstart,
                duration = Duration,
                dry = DryPeriod
                )
              ))
}
