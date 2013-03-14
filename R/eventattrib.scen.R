eventattrib.scen <-
function(scenario,assetid,ctf,gap=2,mindur=3){
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
    scenario <- -99
    if(is.null(assetid)) assetid <- -99
  } else if (is.list(scenario)){
    surfaceflow <- scenario$surfaceflow
    baseflow <- scenario$baseflow
    gwlevel <- scenario$gwlevel
    scenario <- -99
    if(is.null(assetid)) assetid <- -99
  } else {stop("Object has unexpected class")}

  ## Flood events
  flowevent <- eventseq(surfaceflow, thresh = ctf, mingap = gap, mindur=mindur)
  if(!all(is.na(coredata(flowevent))))
    flowevent.attrib <- eventattrib(surfaceflow,flowevent,FUN=mean)
  else return(NULL)
  
  list(
       ##input
       assetid=assetid,
       scenario=scenario,
       ctf=ctf,
       ##output
       events=c(flowevent.attrib$ddd,list(gwlevel=coredata(gwlevel)))
       )

}
