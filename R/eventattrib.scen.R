eventattrib.scen <-
function(scenario,assetid,ctf,gap=2,mindur=3,cache.attribs=FALSE){
  ## Obtain required time series
  if(is.character(scenario)){
    ## TODO: currently assuming that all time series are for the same period,
    ##   and do not need to be subsetted
    ## Surface flow. hardcoded first column
    gauge <- asset.table[assetid,2] #first row is associated with the first asset and so on.
    surfaceflow <- all.hydroinputlist[[scenario]][[paste(gauge,".csv",sep="")]][,1]

    ## Baseflow. Might be NULL
    baseflow <- all.hydroinputlist[[scenario]][[paste(gauge,".csv",sep="")]]$Baseflow

    ## Groundwater level. Might be NULL
    gwlevel <- all.hydroinputlist[[scenario]]$gwlevel.csv
    ##first column is associated with first asset and so on.
    if (!is.null(gwlevel)) gwlevel <- gwlevel[,assetid]

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
  } else {stop("Object has unexpected class")}

  if(cache.attribs & !exists("attrib.cache")) attrib.cache <<- list()
  if(!cache.attribs | is.null(attrib.cache[[scenario]])){
  
    ## Flood events
    flowevent <- eventseq(surfaceflow, thresh = ctf, mingap = gap, mindur=mindur)
    if(!all(is.na(coredata(flowevent))))
      flowevent.attrib <- eventattrib(surfaceflow,flowevent,FUN=mean)
    else return(NULL)

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
                ndays=length(surfaceflow),
                events=c(flowevent.attrib$ddd,list(gwlevel=gwlevel))
                )
    if(cache.attribs)     attrib.cache[[scenario]] <- res
    return(res)
  } else if (cache.attribs & !is.null(attrib.cache[[scenario]])){
    return(attrib.cache[[scenario]])
  } else {
    stop("Unexpected cache.attribs state")
  }
}
