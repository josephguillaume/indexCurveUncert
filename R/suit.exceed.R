## Plot suitability index exceedance curves
suit.exceed <- function(pp.s,pp.b,pr.ex){
  plot(sort(pp.s,decr=T),ppoints(length(pp.s)),type="b",log="y",
       xlab="Suitability index",
       ylab="Probability of exceedance/ Cumulative frequency / Relative rank",
       xlim=c(0,1)#,ylim=c(0,1)
       )
  points(sort(pp.b,decr=T),ppoints(length(pp.b)),col="blue")
  lines(sort(pp.b,decr=T),ppoints(length(pp.b)),col="blue")
  abline(h=pr.ex,lty="dashed")
  abline(v=quantile(pp.s,1-pr.ex))
  abline(v=quantile(pp.b,1-pr.ex),col="blue")
  ## Difference in x% exceedance value
  ## Mostly zeros for 95% non-exceedance = 5% exceedance
  quantile(pp.s,1-pr.ex)-quantile(pp.b,1-pr.ex)
}
