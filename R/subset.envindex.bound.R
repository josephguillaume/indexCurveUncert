subset.envindex.bound <- function(x,subset=T){
    e <- substitute(subset)
    out <- x[sapply(x,function(o) {
        r <- eval(e,o,parent.frame())
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


as.data.frame.envindex.bound <- function(x,row.names=NULL,optional=FALSE,...){
  keep.cols <- which(sapply(x[[1]],length)==1)
  ##keep.cols <- c("assetid","ctf","species","diff.min","diff.max","use.dur")
  do.call(rbind,lapply(x,function(r) as.data.frame(r[keep.cols])))
}

c.envindex.bound <- function(...,recursive=FALSE) {
    y <- NextMethod("c")
    class(y) <- c("envindex.bound",class(y))
    y
}
