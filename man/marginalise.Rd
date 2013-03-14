\name{marginalise}
\alias{marginalise}
\title{
Aggregate discrete results, marginalising over some variables
}
\description{
  Given a \code{data.frame} of discrete results, where some rows
  are the result of variations due to uncertainty, marginalise over
  uncertainty variables to obtain either the agreed answer or "Uncertain".
}
\usage{
marginalise(tabm, form, verbose = F)
}
\arguments{
  \item{tabm}{
    Long format data.frame as produced by melt, with column named \code{value}.
}
  \item{form}{
    Formula passed to case specifying retained variables, in the form \code{var1+var2~.}
}
  \item{verbose}{
    If \code{TRUE}, prints retained and marginalised variables.
}
}
\value{
  A long format data.frame with the retained variables specified in
  \code{form}, and a column \code{(all)} containing either one of the
  original discrete values or "Uncertain" where marginalising resulted
  in more than one value.
}
\author{
Joseph Guillaume
}
\seealso{
}
\examples{
long.df=data.frame(expand.grid(uncert1=1:2,control1=1:3),value=c("a","b","a","a","b","b"))
## Do all rows with a given control1 agree on value, marginalising over uncert1?
marginalise(long.df,control1~.,T)
## Do all rows agree on value?
marginalise(long.df,~.,T)
}
\keyword{ ~kwd1 }
