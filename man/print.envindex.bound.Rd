\name{envindex.bound}
\alias{subset.envindex.bound}
\alias{print.envindex.bound}
\alias{plot.envindex.bound}
\title{Methods for \code{envindex.bound} objects}
\description{
\code{envindex.bound} objects are produced by \code{envindex.diff.qp},
and can be printed, subsetted, plotted.
}
\usage{
\method{print}{envindex.bound}(x, ...)
\method{subset}{envindex.bound}(x,subset=T)
\method{plot}{envindex.bound}(x,y,...,attribs=NA,subset=T,main=NULL)
}
\arguments{
  \item{x}{
    \code{envindex.bound} object, as produced by \code{envindex.diff.qp}
  }
  \item{subset}{
    logical expression indicating elements keep. Defaults to keeping everything
  }
  \item{attribs}{Attributes for which to plot preference curves}
  \item{\dots}{
Ignored}
}
\details{
Plots index curves for given \code{attribs}, with absolute
constraints as background. The curve that yields maximum difference,
such that \code{scenario-baseline>>0}, and \code{scenario>>baseline} is
green. The minimum difference where \code{scenario<<baseline} is red.
}
\value{
\code{data.frame} with columns assetid,ctf,species,diff.min,diff.max and use.dur
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
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
