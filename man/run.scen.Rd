\name{run.scen}
\alias{run.scen}
\alias{run.pref}
\title{
  Calculate measurement index values
}
\description{
  Calculate index value for each measurement of scenario and baseline datasets
}
\usage{
run.scen(xx, dir = "max", attribs.usesduration, data=NULL,calc.mean=TRUE,rep.dur=TRUE)
run.pref(xx, attrib, dir = "max") 
}
\arguments{
  \item{xx}{
    An element of a \code{envindex.bound} object as produced by
    \code{\link{envindex.diff.qp}}
  }
  \item{dir}{
    Either "min" or "max"
  }
  \item{attrib}{
    Attribute for which to calculate preference
  }
  \item{attribs.usesduration}{
    Named vector, where names specify attributes to use, and values
    specify a column to use for duration when \code{use.durs} is
    \code{TRUE}. See \code{\link{envindex.diff.qp}}.
  }
  \item{data}{
    List with elements \code{sidx} and \code{bidx} as provided by \code{envindex.diff.getdata}
    If \code{NULL}, \code{envindex.diff.getdata} is called
  }
  \item{calc.mean}{If \code{TRUE}, i.e. by default, compares mean suitability values between the scen and baseline scenarios. Otherwise, compares total suitability values
  }
  \item{rep.dur}{If \code{TRUE}, attempts to calculate an index per data point by repeating indices by their specified durations. This allows attributes with different frequencies to be compared, but only works if durations are positive integer values.
  }
}
\details{
  Note that order of measurements is not necessarily preserved by
  \code{run.scen}, due to the treatment of attribute durations. \code{run.pref} can
  be used directly if necessary.
}
\value{
  \code{run.pref} returns a linear-piecewise function that returns the
  index value for a given attribute value.
  
  \code{run.scen} returns a list with elements:
  \item{total.diff}{Difference between scenarios with weighted sum. Difference of means if \code{calc.mean=TRUE}, difference of totals otherwise}
  \item{total.diff.attribs}{Total difference between scenarios for each attribute. Difference of means if \code{calc.mean=TRUE}, difference of totals otherwise}
  \item{pp.s}{List of index values for each measurement for each
    attribute for the scenario dataset}
  \item{pp.b}{List of index values for each measurement for each
    attribute for the baseline dataset}
  \item{pd.s}{Index value for each measurement aggregating attributes
    for the scenario dataset}
  \item{pd.b}{Index value for each measurement aggregating attributes
    for the baseline dataset}
}
