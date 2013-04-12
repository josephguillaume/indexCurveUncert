## Plot suitability index exceedance curves
suit.exceed <- function(pp.s,pp.b,pr.ex,add=FALSE,...){
  if(add) {
    points(sort(pp.s,decr=T),ppoints(length(pp.s)))
    lines(sort(pp.s,decr=T),ppoints(length(pp.s)))
  } else {
    plot(sort(pp.s,decr=T),ppoints(length(pp.s)),type="b",
         xlab="Suitability index",
         ylab="Probability of exceedance/ Cumulative frequency / Relative rank",
         xlim=c(0,1),...
         )
  }
  points(sort(pp.b,decr=T),ppoints(length(pp.b)),col="blue")
  lines(sort(pp.b,decr=T),ppoints(length(pp.b)),col="blue")
  if(!missing(pr.ex)){
    abline(h=pr.ex,lty="dashed")
    abline(v=quantile(pp.s,1-pr.ex))
    abline(v=quantile(pp.b,1-pr.ex),col="blue")
    ## Difference in x% exceedance value
    ## Mostly zeros for 95% non-exceedance = 5% exceedance
    quantile(pp.s,1-pr.ex)-quantile(pp.b,1-pr.ex)
  }
}

suit.exceed.diff <- function(pp.s,pp.b,add=FALSE,...){
  stopifnot(length(pp.s)==length(pp.b))
  if(add){
    points(ppoints(length(pp.s)),sort(pp.s,decr=T)-sort(pp.b,decr=T),pch=".")
    lines(ppoints(length(pp.s)),sort(pp.s,decr=T)-sort(pp.b,decr=T))
  } else {
    plot(ppoints(length(pp.s)),sort(pp.s,decr=T)-sort(pp.b,decr=T),
         type="b",
         ylab="Difference in daily suitability index",
         xlab="Probability of exceedance/ Cumulative frequency / Relative rank",
         pch=".",...
         )
    abline(h=0,lty="dashed",col="grey")
  }
}  

## TODO: too messy
suit.exceed2 <- function(xx,attribs.usesduration,...){
  rmax <- lapply(xx,run.scen,dir="max",attribs.usesduration)
  rmin <- lapply(xx,run.scen,dir="min",attribs.usesduration)

  suit.exceed(rmax[[1]]$pd.s,rmax[[1]]$pd.b,...)
  for(x in rmax) suit.exceed(x$pd.s,x$pd.b,add=T)
  for(x in rmin) suit.exceed(x$pd.s,x$pd.b,add=T)  
}

## TODO: too messy
suit.exceed.diff2 <- function(xx,attribs.usesduration,...){
  rmax <- lapply(xx,run.scen,dir="max",attribs.usesduration)
  rmin <- lapply(xx,run.scen,dir="min",attribs.usesduration)

  suit.exceed.diff(rmax[[1]]$pd.s,rmax[[1]]$pd.b,...)
  for(x in rmax) suit.exceed.diff(x$pd.s,x$pd.b,add=T)
  for(x in rmin) suit.exceed.diff(x$pd.s,x$pd.b,add=T)  
}
