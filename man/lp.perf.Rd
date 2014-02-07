\name{lp.perf}
\alias{lp.perf}
\title{
  Extreme index curves by constrained linear programming
}
\description{
  Calculate minimum or maximum difference in suitability between two
  scenarios by varying ordinates of breakpoints of a piece-wise linear index
  curve. Uses linear programming within constraints expressing knowledge
  about behaviour of the index curve.
}
\usage{
lp.perf(xs, ev, bounds = NULL, dir = "min", constr = list(), dur = NULL)
}
\arguments{
  \item{xs}{
    Abcissa of break-points defining piece-wise linear index curve
}
  \item{ev}{
    List of two numeric vectors with data observations for each
    scenario. \code{NULL} if one of the scenarios has no observations.
}
  \item{bounds}{
    List with numeric vectors named \code{lower} and \code{upper}, of
    same length as \code{xs}, defining upper and lower limits of the
    break-point abcissa. Usually produced by a \code{getPrefConstraints}
    function, e.g. \code{\link{getPrefConstraintsLists}}
}
  \item{dir}{
    "min" to produce minimum difference in suitability, "max" to produce
    maximum difference in suitability
}
  \item{constr}{
    \code{data.frame} with four columns \code{a},\code{b},\code{status} ("<="
    or ">="), and
    optionally \code{min.gap} (otherwise it defaults to zero). Each
    row is interpreted as: breakpoints number \code{a} and \code{b}
    satisfy the constraint specified by \code{status}: \code{a <= b -
      min.gap} or \code{a >= b + min.gap}. Usually produced by a \code{getPrefConstraints}
    function, e.g. \code{\link{getPrefConstraintsLists}}
}
  \item{dur}{
    List of two numeric vectors of same length as \code{ev} with weights
    for each observation of each scenario. A common use is to specify
    duration of events.  
}
}
\details{
  The code constructs a linear program of the form:
  \code{A0+A1*y1+A2*y2+...+Ai*yi}, with linear constraints on \code{yi} defined
  by \code{bounds} and \code{constr}.
  
  \code{yi} are the ordinates of the breakpoints of the piece-wise
  linear index curve of abcissa \code{xs}

  \code{Ai} are calculated from the values of
  \code{xs}, \code{ev} and \code{dur}.

  The linear program is then solved using the \code{lpSolveAPI} package.
}
\value{
  List with elements
  \item{obj}{Difference in suitability of second scenario - first
    scenario}
  \item{ys}{Values of the ordinates of the breakpoints of abcissa
    \code{xs}}
  \item{duals}{Dual values of the \code{ys}, as reported by \code{lpSolveAPI}}
}
\author{
Joseph Guillaume
}
\seealso{
\code{\link{envindex.diff.qp}} will generally be used instead as a
wrapper around this function.
}
\examples{
## Index curve with breakpoints at 0,40,100,300
## Two scenarios each with three observations
## The output (suitability of the second scenario - suitability of the
##  first scenario) will be maximised
## Bounds are specified
## Single constraint specifies that the ordinate of the 4th breakpoint
## (x=300) must be less than or equal to the ordinate of the 3rd breakpoint (100).
## The suitabilities of every observation will be multiplied by 100.
  lp.perf(xs=c(0, 40, 100, 300),
        ev=list(c(5,60,170), c(4,70,180)),
        bounds=list(
          lower = c(0.8, 0.8, 0.8, 0),
          upper = c(1, 1, 1,0.2)),
        dir="max",
        constr=data.frame(a = 4, b = 3, status = "<=", min.gap = 0),
        dur=list(c(100,100,100), c(100,100,100))
        )



## One observation of data for one scenario, none for the other
## The output will be minimised. In this case, as the 
##  first scenario is NULL, this amounts to maximising the
##  suitability of the second scenario.
## The suitability of the observation in the second scenario is multiplied by 100
lp.perf(xs=c(0, 40, 100, 300),
        ev=list(NULL, 100),
        bounds=list(
          lower = c(0.8, 0.8, 0.8, 0),
          upper = c(1, 1, 1,0.2)),
        dir="min",
        constr=data.frame(a = 4, b = 3, status = "<=", min.gap = 0),
        dur=list(numeric(0), 100)
        )
}
