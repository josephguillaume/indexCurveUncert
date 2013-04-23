## assume sidx,bidx in global
run.scen <- function(xx,dir="max",attribs.usesduration){
  stopifnot(!is.null(xx$species)) ##FIXME
  weights <- xx[[sprintf("pars.%s.weights",dir)]]
  attribs <- names(weights)
  ## Get event prefs for each attribute
  pp.s <- lapply(attribs,function(attrib)
                 ##run.pref(xx,attrib,dir=dir)(sidx$events[[attrib]])
                 dayPrefsDur(xx,dir,sidx,attrib,attribs.usesduration)
                 )
  names(pp.s) <- attribs
  pp.b <- lapply(attribs,function(attrib)
                 dayPrefsDur(xx,dir,bidx,attrib,attribs.usesduration)
                 )
  names(pp.b) <- attribs
  ## Get day prefs. Days for different attributes must be of same length
  pd.s <- rep(0,ndays)
  for(i in 1:length(weights)) pd.s <- pd.s+weights[i]*pp.s[[i]]
  pd.b <- rep(0,ndays)
  for(i in 1:length(weights)) pd.b <- pd.b+weights[i]*pp.b[[i]]
  ## Total diff
  total.diff <- sum(pd.s)-sum(pd.b)
  list(total.diff=total.diff,
       pp.s=pp.s,pp.b=pp.b,
       pd.s=pd.s,pd.b=pd.b)
}

## TODO: check xx format correct
run.pref <- function(xx,attrib,dir="max"){
  cpt <- index.all[[sprintf("%s_%s.csv", xx$species,attrib)]]
  approxfun(cpt[,1],xx[[sprintf("pars.%s.%s",dir, attrib)]],rule=2)
}
##plot(run.pref(xx,species,"duration",dir="max"),0,1000)

## TODO: maintain order of days
dayPrefsDur <- function(xx,dir,idx,attrib,attribs.usesduration){
  which.dur <- attribs.usesduration[attrib]
  ## TODO: better id number of days
  ndays   <- length(idx$events$gwlevel)
  if(is.na(which.dur)){ ##Already daily
    pp <- run.pref(xx,attrib,dir=dir)(idx$events[[attrib]])
    stopifnot(length(pp)==ndays)
  } else if (xx$use.dur){ ##Treat value as average across days -> repeat
    pp <- unlist(lapply(1:length(idx$events[[attrib]]),
                        function(i) rep(idx$events[[attrib]][i],idx$events[[which.dur]][i])))
    pp <- run.pref(xx,attrib,dir=dir)(pp)
    pp <- c(rep(0,ndays-length(pp)),pp)
  }  else{ ## Treat values as total across days -> split
    ff <- run.pref(xx,attrib,dir=dir)
    pp <- unlist(lapply(1:length(idx$events[[attrib]]),
                        function(i) rep(ff(idx$events[[attrib]][i])/idx$events[[which.dur]][i],
                                        idx$events[[which.dur]][i])))
    pp <- c(rep(0,ndays-length(pp)),pp)    
  }
  pp
}