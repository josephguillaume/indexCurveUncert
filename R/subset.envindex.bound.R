subset.envindex.bound <- function(x,subset=T){
    e <- substitute(subset)
    env <- parent.frame()
    out <- x[sapply(x,function(o) {
        r <- eval(e,o,env)
        r <- r & !is.na(r)
    })]
    class(out) <- class(x)
    out
}

"[.envindex.bound" <- function (x, i)
{
    cl <- oldClass(x)
    y <- NextMethod("[")
    oldClass(y) <- cl
    y
}


as.data.frame.envindex.bound <- function(x,row.names=NULL,optional=FALSE,keep.cols=NULL,...){
  if(is.null(keep.cols)) keep.cols <- which(sapply(x[[1]],length)==1)
  ##keep.cols <- c("assetid","ctf","species","diff.min","diff.max","use.dur")
  do.call(rbind,lapply(x,function(r) as.data.frame(r[keep.cols])))
}

c.envindex.bound <- function(...,recursive=FALSE) {
    y <- NextMethod("c")
    class(y) <- c("envindex.bound",class(y))
    y
}

"*.envindex.bound" <- function(e1,e2){
  if(inherits(e1,"envindex.bound")) {obj=e1;val=e2}
  else { obj=e2;val=e1}
  out<-lapply(obj,function(x){
    x$diff.min<-val*x$diff.min
    x$diff.max<-val*x$diff.max
    x
  })
  class(out) <- class(obj)
  out
}
