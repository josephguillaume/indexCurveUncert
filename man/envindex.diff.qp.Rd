\name{envindex.diff.qp}
\alias{envindex.diff.qp}
\title{
Max/min differences by Quadratic Programming
}
\description{
Calculate the maximum and minimum difference between indices 
}
\usage{
envindex.diff.qp(..., scen, baseline, ecospecies, use.durs = FALSE)
}
\arguments{
  \item{\dots}{
    Arguments passed to \code{\link{eventattrib.scen}}
}
  \item{scen}{
    Scenario to be passed to \code{\link{eventattrib.scen}}, which will
    then be compared to baseline. Can be \code{zoo} object or name of a
    \code{zoo} element in \code{all.hydroinputlist}.
}
  \item{baseline}{
    Scenario to be passed to \code{\link{eventattrib.scen}}, which will
    be used as baseline.
}
  \item{ecospecies}{
    Character vector of species names, as listed within \code{index.all}
}
  \item{use.durs}{
%%     ~~Describe \code{use.durs} here~~
}
}
\details{
Calculates event attributes for both \code{scen} and
\code{baseline}. Calculates minimum and maximum difference between
scenarios, for each species and treating event attributes as means and
sums, by varying preference curves within bounds defined by \code{getPrefConstraints} and varying weights within bounds defined by \code{getWeightConstraints}
}
\value{
  Object of class \code{envindex.bound}
List of model runs with elements:
\item{assetid }{}
\item{ctf}{}
\item{species}{}
\item{use.dur}{}
\item{diff.min}{}
\item{diff.max}{}
\item{pars.min.weights}{Numeric vector of weights}
\item{pars.min.attrib}{Numeric vector of ordinates of breakpoints for
  each attrib}
\item{pars.max.weights}{}
\item{pars.max.attrib}{}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Joseph Guillaume
}
\seealso{
  Uses getPrefConstraints,getWeightConstraints
Calls lp.weights,lp.perf
}
\examples{
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
