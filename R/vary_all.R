getSeqCtfs10 <- function(assetid) {
  ctfs <- asset.table[assetid,4:6]
  seq.ctfs <- seq(min(ctfs)*0.25,max(ctfs)*1.25,length.out=10)
  ##seq.ctfs <- ctfs[2]
  seq.ctfs <- sort(unique(c(seq.ctfs,as.numeric(ctfs))))
}

vary_all <- function(assets,do.par=T,getSeqCtfs,...){
  seq.ctfs <- lapply(assets,getSeqCtfs)
  if (!do.par){
    cat("Starting runs\n")
    start <- proc.time()
    all.diffs2 <-
      lapply(assets,
             function(assetid,seq.ctfs,...){
               seq.ctfs <- seq.ctfs[[assetid]]
               all.diffs <- NULL
               for(ctf in seq.ctfs){ ##9 sec each
                                        #st <- proc.time()
                 all.diffs <-
                   c(all.diffs,
                     envindex.diff.qp(assetid=assetid,ctf=ctf,...
                                      ))
                                        #print(proc.time()-st)
               }
               all.diffs
             },seq.ctfs=seq.ctfs,...)
    cat("Finished runs\n")
    print(proc.time()-start)
  } else {
    library(snow)
    cl <- makeCluster(3, type = "SOCK")
    clusterEvalQ(cl,library(indexCurveUncert))

    toexport <- c("all.hydroinputlist","asset.table", ##Hydro essentials
                  "index.all","attribs.usesduration", ##Index essentials
                  "pref.bounds","pref.smooth","pref.comp","pref.monoton", ##getPrefConstraintsLists
                  "weight.bounds","weight.comp", ##getWeightConstraintsLists
                  "getWeightConstraints","getPrefConstraints", ## User overrides of default
                  "cached.pref", #getPrefConstraintsCached,getPrefConstraintsMergeWithCache
                  "approxes.all" #getPrefConstraintsMultIndex
                  )
    ##setdiff(ls(),toexport) ##what won't be exported
    toexport <- intersect(toexport,ls(envir=.GlobalEnv))
    ##print(toexport)
    clusterExport(cl,toexport)

    print("Starting runs")
    start <- proc.time()
    all.diffs2 <-
      clusterApplyLB(cl,
                     assets,
                     function(assetid,seq.ctfs,...){
                       seq.ctfs <- seq.ctfs[[assetid]]
                       all.diffs <- NULL
                       for(ctf in seq.ctfs){ ##9 sec each
                         ##st <- proc.time()
                         all.diffs <-
                           c(all.diffs,
                             envindex.diff.qp(assetid=assetid,ctf=ctf,...
                                              ))
                         ##print(proc.time()-st)
                       }
                       all.diffs
                     },seq.ctfs=seq.ctfs,...)
    cat("Finished runs\n")
    print(proc.time()-start)

    stopCluster(cl)
  }
  all.diffs <- do.call(c,all.diffs2)
  class(all.diffs) <- c("envindex.bound",class(all.diffs))
  all.diffs
}

## Drop all results for the named species & redo
redo_diffs <- function(all.diffs,ecospecies,...){
  all.diffs <- subset(all.diffs,!species %in% ecospecies)
  all.diffs <- c(all.diffs,vary_all(...,ecospecies=ecospecies))
}
