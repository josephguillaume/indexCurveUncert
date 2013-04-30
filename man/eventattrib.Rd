\name{eventattrib}
\alias{eventattrib}
\title{
Calculate attributes of given events
}
\description{
  Calculate attributes for events, including date of start, end and
  middle, month of start, length/duration,length of dry period.
}
\usage{
eventattrib(X, events, FUN = mean, ...)
}
\arguments{
  \item{X}{
    \code{\link[zoo]{zoo}} object of streamflow
}
  \item{events}{
    \code{\link[zoo]{zoo}} object defining events, as produced by \code{\link{eventseq}}
}
  \item{FUN}{
    function applied to streamflow values of each event, and returned as
    variable  \code{xValues}
}
  \item{\dots}{
    Ignored.
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
Usually called by \code{\link{eventattrib.scen}}
}
\examples{
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
