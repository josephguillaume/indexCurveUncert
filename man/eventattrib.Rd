\name{eventattrib}
\alias{eventattrib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate attributes of given events
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eventattrib(X, events, FUN = mean, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{events}{
%%     ~~Describe \code{events} here~~
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (X, events, FUN = mean, ...) 
{
    stopifnot(inherits(X, "zoo"))
    xValues <- eventapply(X, events = events, FUN = FUN, ...)
    xValuesend <- eventapply(X, events = events, FUN = FUN, TIMING = "end")
    xLengths <- eventapply(X, events = events, FUN = NROW, TIMING = "middle", 
        by.column = FALSE)
    midTimeComponents <- as.POSIXlt(time(xLengths))
    startTimeComponents <- as.POSIXlt(time(xValues))
    Monthstart = startTimeComponents$mon + 1
    Duration = coredata(xLengths)
    DryPeriod = preInterEventDuration(events)
    return(list(events = events, xValues = xValues, xValuesend = xValuesend, 
        xLengths = xLengths, midTimeComponents = midTimeComponents, 
        startTimeComponents = startTimeComponents, Monthstart = Monthstart, 
        Duration = Duration, DryPeriod = DryPeriod, ddd = data.frame(StartTime = time(xValues), 
            EndTime = time(xValuesend), Value = coredata(xValues), 
            Yearmid = midTimeComponents$year + 1900, Monthstart = Monthstart, 
            Duration = Duration, DryPeriod = DryPeriod)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
