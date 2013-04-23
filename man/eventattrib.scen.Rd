\name{eventattrib.scen}
\alias{eventattrib.scen}
\title{
  Calculate event attributes for a given flow scenario
}
\description{
  Define events and call \code{\link{eventattrib}} to calculate event attributes
}
\usage{
eventattrib.scen(scenario, assetid, ctf, gap = 2, mindur = 3,cache.attribs=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scenario}{
    Can be \code{zoo} object or name of a
    \code{zoo} element in \code{all.hydroinputlist}
}
\item{assetid}{
  Index of asset in \code{asset.table}
}
\item{ctf}{
  See \code{\link{eventseq}}
}
\item{gap}{
  See \code{\link{eventseq}}
}
\item{mindur}{
  See \code{\link{eventseq}}
}
\item{cache.attribs}{
  If \code{TRUE} saves and retrieves results from each scenario in
  global \code{attrib.cache}. Saves computation time when multiple runs with same
  scenarios are made.
}
}
\details{
  
}
\value{
  \item{assetid }{assetid input}
  \item{scenario}{scenario input}
  \item{ctf}{Description of 'comp2}
  \item{events}{List of of values for each attribute that will be used
    by envindex.diff.qp. It must not contain NAs. Modify
    \code{eventattrib.scen} to deal with NAs if necessary.}
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
  Called by \code{\link{envindex.diff.qp}}
  
Calls \code{\link{eventattrib}}, \code{\link{eventseq}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
