## TODO: cope with different numbers of parameters for different species
## TODO: cope with different id.vars
what.weights <- function(x){
  do.call(rbind, lapply(x,
                        function(r)
                        cbind(as.data.frame(r[c("assetid", "ctf", "species", "use.dur")]),
                              rbind(
                                    c(run.type="min",
                                      diff=r$diff.min,
                                      r$"pars.min.weights"),
                                    c(
                                      run.type="max",
                                      diff=r$diff.max,
                                      r$"pars.max.weights")
                                    ))
                        ))
}
