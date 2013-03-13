print.envindex.bound <-
function(x,...) {
    print(do.call(rbind,lapply(x,function(r) r[c("assetid","ctf","species","diff.min","diff.max","use.dur")])))
}
