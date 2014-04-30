run.scen <- function(xx,dir="max",attribs.usesduration,data=NULL,calc.mean=TRUE,rep.dur=TRUE){
  stopifnot(!is.null(xx$species)) ##FIXME
  ##It is more efficient to avoid recalculating data, 
  ## but if it isn't specified, calculate it
  if(is.null(data)){
   data=envindex.diff.getdata(scen=xx$scen,baseline=xx$baseline,
                        assetid=xx$assetid,ctf=xx$ctf)
  }
  weights <- xx[[sprintf("pars.%s.weights",dir)]]
  attribs <- names(weights)
  ## Get event prefs for each attribute
  pp.s <- lapply(attribs,function(attrib)
                 ##run.pref(xx,attrib,dir=dir)(data$sidx$events[[attrib]])
                 dayPrefsDur(xx,dir,data$sidx,attrib,attribs.usesduration,calc.mean,rep.dur)
                 )
  names(pp.s) <- attribs
  pp.b <- lapply(attribs,function(attrib)
                 dayPrefsDur(xx,dir,data$bidx,attrib,attribs.usesduration,calc.mean,rep.dur)
                 )
  names(pp.b) <- attribs
  ## Get day prefs. Days for different attributes must be of same length
  if(rep.dur){
    pd.s <- rep(0,data$sidx$ndays)
    for(i in 1:length(weights)) pd.s <- pd.s+weights[i]*pp.s[[i]]
    pd.b <- rep(0,data$bidx$ndays)
    for(i in 1:length(weights)) pd.b <- pd.b+weights[i]*pp.b[[i]]
  } else {
    ## Return a total given that lengths of data might not match
    pd.s = sum(weights * sapply(pp.s,sum))
    pd.b = sum(weights * sapply(pp.b,sum))
  }
  list(total.diff=sum(pd.s)-sum(pd.b),
       total.diff.attribs=sapply(pp.s,sum)-sapply(pp.b,sum),
       pp.s=pp.s,pp.b=pp.b,
       pd.s=pd.s,pd.b=pd.b)
}

## TODO: check xx format is valid
run.pref <- function(xx,attrib,dir="max"){
  bkpts <- xx[[sprintf("bkpts.%s",attrib)]]
  if(is.null(bkpts)){
   ##Only for backwards compatibility
   warning(sprintf("envindex.bound element doesn't have element bkpts.%s, searching in index.all",attrib))
   cpt <- index.all[[sprintf("%s_%s.csv", xx$species,attrib)]]
   return(approxfun(cpt[,1],xx[[sprintf("pars.%s.%s",dir, attrib)]],rule=2))
  }
  approxfun(bkpts,xx[[sprintf("pars.%s.%s",dir, attrib)]],rule=2)
}
##plot(run.pref(xx[[1]],"duration",dir="max"),0,1000)

## TODO: maintain order of days with rep.dur
dayPrefsDur <- function(xx,dir,idx,attrib,attribs.usesduration,calc.mean=TRUE,rep.dur=FALSE){
  which.dur <- attribs.usesduration[attrib]
  
  if(rep.dur && !is.na(which.dur) && (any(idx$events[[which.dur]]<1) || any(floor(idx$events[[which.dur]])!=idx$events[[which.dur]]))){
    stop("The duration attribute must be a positive integer to use rep.dur")
  }
  ## TODO: better id number of days
  if(is.na(which.dur)){ ##Already daily
    pp <- run.pref(xx,attrib,dir=dir)(idx$events[[attrib]])
    stopifnot(length(pp)<=idx$ndays)
    ## Pad difference between actual events and ndays, for NAs
    if(rep.dur) pp <- c(pp,rep(0,idx$ndays-length(idx$events[[attrib]])))
  } else if (!rep.dur && xx$use.dur){
     pp <- run.pref(xx,attrib,dir=dir)(idx$events[[attrib]])*idx$events[[which.dur]]
  } else if (!rep.dur && !xx$use.dur){
     pp <- run.pref(xx,attrib,dir=dir)(idx$events[[attrib]])
  } else if (rep.dur && xx$use.dur){ ##Treat value as average across days -> repeat
     pp <- unlist(lapply(1:length(idx$events[[attrib]]),
                        function(i) rep(idx$events[[attrib]][i],idx$events[[which.dur]][i])))
     pp <- run.pref(xx,attrib,dir=dir)(pp)
     pp <- c(rep(0,idx$ndays-length(pp)),pp)
   } else if (rep.dur && !xx$use.dur){## Treat values as total across days -> split
     ff <- run.pref(xx,attrib,dir=dir)
     pp <- unlist(lapply(1:length(idx$events[[attrib]]),
                        function(i) rep(ff(idx$events[[attrib]][i])/idx$events[[which.dur]][i],
                                        idx$events[[which.dur]][i])))
     stopifnot(length(pp)>0)
     pp <- c(rep(0,idx$ndays-length(pp)),pp)
   }
  if(calc.mean) pp<-pp/idx$ndays*100
  as.numeric(pp)
}
