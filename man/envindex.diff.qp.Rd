\name{envindex.diff.qp}
\alias{envindex.diff.qp}
\title{
Max/min differences by Quadratic Programming
}
\description{
Calculate the maximum and minimum difference between indices 
}
\usage{
envindex.diff.qp(..., scen, baseline, ecospecies, use.durs = FALSE,attribs.usesduration=c("timing"="duration","duration"="duration","dry"="duration"),calc.mean=TRUE)
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
    If \code{TRUE}, will multiple index values by duration specified in
    \code{attribs.usesduration}, e.g. multiplying by length of event, or
    attributing mean monthly value to every day in a month
  }
  \item{attribs.usesduration}{
    Named vector, where names specify attributes to use, and values
    specify a column to use for duration when \code{use.durs} is
    \code{TRUE}.
    The named attribute will be looked up in the results from
    \code{\link{eventattrib.scen}} and to look up CPTs from
    \code{index.all} and \code{approxes.all}.
    To not use an attribute for a particular species, either do not
    provide a CPT such that \code{index.all$species_attrib.csv=NULL}, or
    constrain the weight of the attribute to zero using \code{\link{getWeightConstraints}}.
  }
  \item{calc.mean}{
    If \code{TRUE}, i.e. by default, compares mean suitability values
  between the \code{scen} and \code{baseline} scenarios. Otherwise,
  compares total suitability values
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
\item{diff.min}{Minimum difference between scenarios, as \code{scen}-\code{baseline}}
\item{diff.max}{Maximum difference between scenarios, as \code{scen}-\code{baseline}}
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

## Return the data for given scenarios + ndays
## named list of functions that returns vector of data for given scenario
attrib.fns=list(
  salinity=function(scenario,...) {
    if(scenario=="scenario") {
      return(100)
    } else if(scenario=="baseline"){
      return(50)
    } else if(scenario=="none") {
      ## Null means no data
      return(NULL)
    } else stop("Unrecognised scenario name")
  },
  ndays=function(...) return(1)
  )
## Define the x values of the preference curves
## named list of single column data.frames
index.all=list("our.species_salinity.csv"=data.frame(X1=c(0,40,100,300)))

## Define the attributes that will be used (and the 'duration' of each measurement)
attribs.usesduration=c(salinity=FALSE)

## Defines the way constraints will be looked for
getWeightConstraints<-getWeightConstraintsLists
getPrefConstraints<-getPrefConstraintsLists

## Finds the minimum and maximum using linear programming - returns an envindex.bound object
x=envindex.diff.qp(assetid=NA,ctf=NA,scen="scenario",baseline="none",ecospecies="our.species",attribs.usesduration=attribs.usesduration)

x
plot(x)

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
