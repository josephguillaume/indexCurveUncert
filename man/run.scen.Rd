\name{run.scen}
\alias{run.scen}
\title{
  Calculate measurement index values
}
\description{
  Calculate index value for each measurement of scenario and baseline datasets
}
\usage{
run.scen(xx, dir = "max", attribs.usesduration)
}
\arguments{
  \item{xx}{
An element of a \code{envindex.bound} object as produced by
\code{\link{envindex.diff.qp}}
}
  \item{dir}{
    Either "min" or "max"
}
\item{attribs.usesduration}{
      Named vector, where names specify attributes to use, and values
    specify a column to use for duration when \code{use.durs} is
    \code{TRUE}. See \code{\link{envindex.diff.qp}}.
}
}
\details{
  Uses \code{sidx} and \code{bidx} in global environment, as set by
  \code{\link{envindex.diff.getdata}}

  Note that order of measurements is not preserved.
}
\value{
  List with elements
  \item{total.diff}{Total difference between scenarios}
  \item{pp.s}{List of index values for each measurement for each
    attribute for the scenario dataset}
  \item{pp.b}{List of index values for each measurement for each
    attribute for the baseline dataset}
  \item{pd.s}{Index value for each measurement aggregating attributes
    for the scenario dataset}
  \item{pd.b}{Index value for each measurement aggregating attributes
    for the baseline dataset}
}
