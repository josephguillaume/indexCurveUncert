getSeqCtfs10 <- function(assetid) {
  ctfs <- asset.table[assetid,4:6]
  seq.ctfs <- seq(min(ctfs)*0.25,max(ctfs)*1.25,length.out=10)
  ##seq.ctfs <- ctfs[2]
  seq.ctfs <- sort(unique(c(seq.ctfs,as.numeric(ctfs))))
}

vary_all <- function(assets,do.par=T,getSeqCtfs,cache.attribs=T,...){
  seq.ctfs <- lapply(assets,getSeqCtfs)
  if(cache.attribs && !exists("attrib.cache")) attrib.cache <<- list()
  if (!do.par){
    cat("Starting runs\n")
    start <- proc.time()
    all.diffs2 <-
      lapply(assets,
             function(assetid,seq.ctfs,...){
               seq.ctfs <- seq.ctfs[[which(assets==assetid)]]
               all.diffs <- NULL
               for(ctf in seq.ctfs){ ##9 sec each
                                        #st <- proc.time()
                 all.diffs <-
                   c(all.diffs,
                     envindex.diff.qp(assetid=assetid,ctf=ctf,cache.attribs=cache.attribs,...
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
                  "approxes.all", #getPrefConstraintsMultIndex
                  "attrib.cache", # eventattrib.scen
                  "assets" ## seq.ctfs ordering
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
                       seq.ctfs <- seq.ctfs[[which(assets==assetid)]]
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
                     },seq.ctfs=seq.ctfs,cache.attribs=cache.attribs,...)
    cat("Finished runs\n")
    print(proc.time()-start)

    ## Retrieve cached attributes for all assets
    if(cache.attribs){
      caches <- clusterEvalQ(cl,attrib.cache)
      ##new.attribs <- setdiff(unique(do.call(c,lapply(caches,names))),names(attrib.cache))
      for(cc in caches){
        new.attribs <- setdiff(unique(names(cc)),names(attrib.cache))
        if(length(new.attribs)>0) attrib.cache <<- modifyList(attrib.cache,cc[new.attribs])
      }
    }

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
