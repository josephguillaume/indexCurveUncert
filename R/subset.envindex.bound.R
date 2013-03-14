subset.envindex.bound <- function(x,subset=T){
    e <- substitute(subset)
    out <- x[sapply(x,function(o) {
        r <- eval(e,o,parent.frame())
        r <- r & !is.na(r)
    })]
    class(out) <- class(x)
    out
}
