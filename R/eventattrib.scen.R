eventattrib.scen <-
function(scenario,assetid,ctf,gap=2,mindur=3){
  if(is.character(scenario)){
    gauge <- asset.table[assetid,2] #first row is associated with the first asset and so on.
    surfaceflow <- all.hydroinputlist[[scenario]][[paste(gauge,".csv",sep="")]][,1]
  } else if(is.zoo(scenario)){
    surfaceflow <- scenario
    scenario <- -99
    if(is.null(assetid)) assetid <- -99
  } else {stop("Object has unexpected class")}
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
       events=flowevent.attrib$ddd
       )

}
