\name{getPrefConstraints}
\alias{getWeightConstraints}
\alias{getWeightConstraintsNull}
\alias{getWeightConstraintsLists}
\alias{getPrefConstraintsCached}
\alias{getPrefConstraintsMultIndex}
\alias{getPrefConstraintsMergeWithCache}
\alias{getPrefConstraintsLists}
\title{
  Return constraints to be used on index curve and weight breakpoints
}
\description{
  Defines functions that can be called to obtain constraints on preferences
  or weights. 
}
\usage{
getWeightConstraintsNull(species, attribs)
getWeightConstraintsLists(species, attribs)
getPrefConstraintsMultIndex(species, attrib)
getPrefConstraintsLists(species, attrib)
getPrefConstraintsCached(species, attrib, backup = NA)
getPrefConstraintsMergeWithCache(species, attrib, extra)
}
\arguments{
  \item{species}{
    Species name, as specified in \code{ecospecies} when calling \code{\link{envindex.diff.qp}}.
}
  \item{attribs}{
    Attribute names for which weights will be calculated, as specified in names of \code{attribs.usesduration}
    when calling \code{\link{envindex.diff.qp}}.
  }
  \item{attrib}{
    Attribute name for which preference curve will be calculated, as
    specified in names of \code{attribs.usesduration} when calling \code{\link{envindex.diff.qp}}.
  }    
  \item{backup}{
    Another \code{getPrefConstraints} function that will be used if the
    species and attribute is not found in the cache
  }
  \item{extra}{
    Another \code{getPrefConstraints} function the constraints from
    which will be merged with those found in the cache. Tighter bounds
    are kept. Conflicting relative constraints are not checked, and may
    cause errors from lpSolve.
    }
}
\details{
\code{\link{envindex.diff.qp}} and \code{\link{plot.envindex.bound}}
call functions called \code{getWeightConstraints} and
\code{getPrefConstraints} in the global environment. If they cannot find
them, they set them to \code{getWeightConstraintsLists} and
\code{getPrefConstraintsMultIndex} respectively.
  
\code{getWeightConstraintsNull} sets no constraints on relative values
of weights, and allows weights to take any value in [0,1] (subject to
sum of weights=1)

\code{getWeightConstraintsLists} looks for a number of lists in the
global environment
\itemize{
\item{weight.comp}{List with names corresponding to species and elements
  of the form
  \code{data.frame(a="w1",b="w2",dir="<=",min.gap=0.01)}. Weight
  \code{a} is related to weight \code{b} by direction \code{dir} with a
  minimum gap of \code{min.gap}. e.g. \code{w1+0.01<=w2}. The names of \code{a} and \code{b} must correspond
  to valid names of  \code{attribs.usesduration} as passed to  \code{\link{envindex.diff.qp}}
}
\item{weight.bounds}{List with names corresponding to species and
  elements of the form
  \code{data.frame(a="w1",min.weight=0.1,max.weight=NA)}. Weight of the
  attribute named \code{a} must be between \code{min.weight} and
  \code{max.weight}. If a value is \code{NA}, no constraint is
  set. e.g. code{0.1<w1}, with no upper bound.
}
}

\code{getPrefConstraintsLists} looks for a number of lists in the global
environment, and creates empty lists if it does not find them. Each list
has names of form \code{species_attrib.csv} where \code{species} and
elements as data.frames
\code{attrib} are as specified to \code{\link{envindex.diff.qp}} using \code{ecospecies} and
\code{attribs.usesduration} respectively.
\itemize{
\item{pref.bounds}{e.g. \code{data.frame(min.x=12,max.x=Inf,min.y=NA,max.y=0)}}
\item{pref.monoton}{Monotonicity constraints
  e.g. \code{data.frame(min.x=5,max.x=12,dir=-1,min.step=0)}}
\item{pref.smooth}{Smoothness constraint
  \code{data.frame(min.x=0,max.x=100,min.step=-0.02,max.step=0.02)}}
\item{pref.comp}{Comparison constraint e.g. \code{data.frame(min.x1=2,max.x1=2,min.x2=4,max.x2=4,dir="<",min.gap=0.1)}}
}
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
Joseph Guillaume
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
