preInterEventDuration <-
function(evq)
{
    if (inherits(evq, "zoo"))
        stopifnot(is.regular(evq, strict = TRUE))
    interCounter <- cumsum(is.na(coredata(evq)))
    vals <- tapply(interCounter, coredata(evq), FUN = head,1)
    c(vals[1], diff(vals))
}
