\name{eventattrib.scen}
\alias{eventattrib.scen}
\title{
  Calculate event attributes for a given flow scenario
}
\description{
  Define events and call \code{\link{eventattrib}} to calculate event attributes
}
\usage{
eventattrib.scen(scenario, assetid, ctf, gap = 2, mindur = 3)
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
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{assetid }{assetid input}
  \item{scenario}{scenario input}
  \item{ctf}{Description of 'comp2}'
  \item{events}{Description of 'comp2}'
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
