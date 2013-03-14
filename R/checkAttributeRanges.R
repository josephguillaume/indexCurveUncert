
checkAttributeRanges <- function(scenario,specieslist,attribs=c("duration","dry")){
  ## Check max values that cpts have to go to
  maxes <- NULL
  for(assetid in 1:nrow(asset.table)){
    ctfs <- asset.table[assetid,4:6]
    ##For longest durations
    ##ctf <- 0.25*min(ctfs)
    ##For longest dry periods
    ##ctf <- 1.25*max(ctfs)
    seq.ctfs <- seq(min(ctfs)*0.25,max(ctfs)*1.25,length.out=10)
    seq.ctfs <- sort(unique(c(seq.ctfs,as.numeric(ctfs))))
    ctf <- seq.ctfs[11]
    ##ctf <- max(seq.ctfs)
    ee <- eventattrib.scen(scenario=scenario,assetid=assetid,ctf=ctf)
    if(!is.null(ee)) maxes <-
      rbind(maxes,
            data.frame(assetid=assetid,ctf=ctf,t(sapply(ee$events[attribs],max,na.rm=T)))
            )
  }
  ##maxes

  ## Check max values of cpts index.all
  maxes.cpt <- NULL
  for(s in specieslist)
    maxes.cpt <-
      rbind(maxes.cpt,
            data.frame(species=s,
                       t(
                         sapply(attribs,function(a)
                                max(index.all[[sprintf("%s_%s.csv", s,a)]][,1],na.rm=T))
                         )))
  ##maxes.cpt
  list(maxes,maxes.cpt)
}
