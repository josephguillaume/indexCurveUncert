eventattrib.scen <-
function(scenario,assetid,ctf,gap=2,mindur=3,cache.attribs=FALSE){
  ## Obtain required time series
  if (is.character(scenario)) {
    gauge <- asset.table[assetid, 2] #first row is associated with the first asset and so on.
    bore <- asset.table$Bore[assetid]
    ## Groundwater level. Might be NULL
    gwlevel <- all.hydroinputlist[[scenario]]$gwlevel.csv[,as.character(bore)]
    ## Surface flow. hardcoded first column
    surfaceflow <- all.hydroinputlist[[scenario]][[paste(gauge, 
                                                         ".csv", sep = "")]][, 1]
    ## Combine to ensure same length and same NAs
    combined <- list(gwlevel = gwlevel,
                     surfaceflow = surfaceflow,
                     ## Baseflow. Might be NULL
                     baseflow = all.hydroinputlist[[scenario]][[paste(gauge, 
                       ".csv", sep = "")]]$Baseflow,
                     all = F)
    combined <- do.call(cbind.zoo, combined[!sapply(combined,is.null)])
    ## Place Nas in all non-complete rows. Later omitted for gw,and don't appear in events
    coredata(combined)[!complete.cases(combined),] <- NA

    if (!is.null(gwlevel) && !identical(index(gwlevel), index(surfaceflow))) {
      warning(sprintf("In %s with assetid %d gwlevel and surfaceflow time periods do not match, automatically trimming\nNew time range %s to %s", 
                      scenario, assetid, min(index(combined)), max(index(combined))))
    }

    surfaceflow <- combined$surfaceflow
    baseflow <- combined$baseflow
    gwlevel <- combined$gwlevel
    
  } else if(is.zoo(scenario)){
    surfaceflow <- scenario
    baseflow <- NULL
    gwlevel <- NULL
    scenario <- digest(scenario)
    if(is.null(assetid)) assetid <- -99
  } else if (is.list(scenario)){
    surfaceflow <- scenario$surfaceflow
    baseflow <- scenario$baseflow
    gwlevel <- scenario$gwlevel
    scenario <- digest(scenario)
    if(is.null(assetid)) assetid <- -99
    ## Fail if don't have same time indices
    stopifnot(is.null(gwlevel) | identical(index(gwlevel),index(surfaceflow)))
  } else {stop("Object has unexpected class")}

  cache.id <- paste(scenario,assetid,ctf,sep=" ")
  
  if(cache.attribs && !exists("attrib.cache")) attrib.cache <<- list()
  if (cache.attribs) cached <- attrib.cache[[cache.id]]
  
  ## If not using caching or run isn't in cache. is.null is only checked if cache.attribs
  if(!cache.attribs | (cache.attribs && is.null(cached))){
  
    ## Flood events. If no events, flowevent.attrib may have NULLs in list
    flowevent <- eventseq(surfaceflow, thresh = ctf, mingap = gap, mindur=mindur)
    flowevent.attrib <- eventattrib(surfaceflow,flowevent,FUN=mean)

    ##gwlevel
    gwlevel <- coredata(gwlevel)
    ## TODO: better way of dealing with NAs?
    gwlevel <- na.omit(gwlevel)

    res <- list(
                ##input
                assetid=assetid,
                scenario=scenario,
                ctf=ctf,
                ##output
                ##checked to be same as length(gwlevel) and have same number of NAs as gwlevel
                ndays=length(na.omit(coredata(surfaceflow))), 
                events=c(flowevent.attrib$ddd,list(gwlevel=gwlevel))
                )
    if(cache.attribs)     attrib.cache[[cache.id]] <<- res
    return(res)
  } else if (cache.attribs && !is.null(cached)){
    return(cached)
  } else {
    stop("Unexpected cache.attribs state")
  }
}
