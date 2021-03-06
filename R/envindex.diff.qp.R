envindex.diff.qp <-
function(...,scen,baseline,ecospecies,use.durs=FALSE,
         attribs.usesduration=c("timing"="duration","duration"="duration","dry"="duration"),
         calc.mean=TRUE
         ){
    sidx <- eventattrib.scen(...,scenario=scen)
    bidx <- eventattrib.scen(...,scenario=baseline)

    if(!exists("getPrefConstraints")){
      warning("function getPrefConstraints not set, using getPrefConstraintsMultIndex")
      assign("getPrefConstraints",getPrefConstraintsMultIndex,env=.GlobalEnv)
    }
    if(!exists("getWeightConstraints")){
      warning("function getWeightConstraints not set, using getWeightConstraintsLists")
      assign("getWeightConstraints",getWeightConstraintsLists,env=.GlobalEnv)
    }

    if(!is.character(scen)) {
        flow.scen <- scen
        scen <- digest(scen)
    }
    if(!is.character(baseline)) {
        flow.base <- baseline
        baseline <- digest(baseline)
    }

    ##TODO: some requested ctf values/scenarios will have no results transparently
    if(is.null(sidx) |is.null(bidx)) return(NULL)
    stopifnot(!is.null(sidx$events)) ## should only be a single result
    stopifnot(identical(bidx[c("assetid","ctf")],
                        sidx[c("assetid","ctf")]
                        ))
    all.prefs <- NULL
    ## Run for each dur
    for(use.dur in use.durs){
        ## Run for each species
        for(species in ecospecies){
            ## For each attribute, get min and max preference curve results
            prefs <- lapply(names(attribs.usesduration),function(attrib){
                ## For this attribute, extract events and current cpt (index curve abcissa)
                ev <- list(bidx$events[[attrib]],
                           sidx$events[[attrib]])
                lapply(ev[!sapply(ev,is.null)],na.fail)
                cpt <- index.all[[sprintf("%s_%s.csv", species,attrib)]]
                ## Breakpoint abcissa must be unique and sorted
                stopifnot(identical(cpt[,1],sort(unique(cpt[,1]))))
                ##Attributes will not be considered if cpt is NULL
                if(is.null(cpt)) return(NULL)

                ## Specify durations to be used to multiply index values
                ##  if requested by use.dur and available in attribs.usesduration
                which.dur <- attribs.usesduration[attrib]
                if (use.dur & !is.na(which.dur))
                  dur <- list(bidx$events[[which.dur]],
                              sidx$events[[which.dur]])
                else dur <- NULL

                ## Divide total by number of days
                ## *100 to minimise precision issues TODO: is this enogh
                if(calc.mean){
                    if(is.null(dur)) dur <- list(rep(1/bidx$ndays*100,length(ev[[1]])),
                                                 rep(1/sidx$ndays*100,length(ev[[2]])))
                    else dur <- list(dur[[1]]/bidx$ndays*100,dur[[2]]/sidx$ndays*100)
                }

                ## Calculate results for min-diff and max-diff preference curves
                constr <- getPrefConstraints(species,attrib)
                p.max <- lp.perf(cpt[,1],ev,bounds=constr$bounds,
                                 dir="max",constr=constr$constr,dur=dur)
                p.min <- lp.perf(cpt[,1],ev,bounds=constr$bounds,
                                 dir="min",constr=constr$constr,dur=dur)

                c(sidx[c("assetid","ctf")],
                  list(use.dur=use.dur,
                       species=species,
                       attrib=attrib,
                       max=p.max,
                       min=p.min,
                       constr=constr
                       ))
            }) ## attrib
            names(prefs) <- names(attribs.usesduration)
            prefs <- prefs[!sapply(prefs,is.null)]
            if(length(prefs)==0){
                w.min=list(obj=NA)
                w.max=list(obj=NA)
                pars.min <- NA
                pars.max <- NA
            } else {
                constr <- getWeightConstraints(species,names(prefs))
                w.min <- lp.weights(sapply(prefs,function(x) x$min$obj),
                                    "min",
                                    bounds=constr$bounds,
                                    constr=constr$constr)
                w.max <- lp.weights(sapply(prefs,function(x) x$max$obj),
                                    "max",
                                    bounds=constr$bounds,
                                    constr=constr$constr)

                ## All model parameters
                bkpts <- lapply(prefs,function(x) index.all[[sprintf("%s_%s.csv", x$species, x$attrib)]][,1])
                pars.min <- c(list(weights=w.min$ws),
                              lapply(prefs,function(x) x$min$ys))
                pars.max <- c(list(weights=w.max$ws),
                              lapply(prefs,function(x) x$max$ys))
            }
            all.prefs <- c(all.prefs,list(c(sidx[c("assetid","ctf","scenario")],
                                            list(baseline = bidx$scenario,
                                                 species=species,
                                                 use.dur=use.dur,
                                                 diff.min=w.min$obj,
                                                 diff.max=w.max$obj,
                                                 diff.min.attribs=sapply(prefs,function(x) x$min$obj),
                                                 diff.max.attribs=sapply(prefs,function(x) x$max$obj)
                                                 ),
                                            bkpts=bkpts,
                                            pars.min=pars.min,
                                            pars.max=pars.max
                                            )))

        } ## species
    }##use.dur
    class(all.prefs) <- c("envindex.bound",class(all.prefs))
    invisible(all.prefs)
    ##do.call(rbind,lapply(all.prefs,function(x) x[c("assetid","ctf","species","diff.min","diff.max")]))
}


envindex.diff.getdata <-
function(...,scen,baseline){
  ## FIXME: Universal assign is for backwards compatibility
  sidx <<- eventattrib.scen(...,scenario=scen)
  bidx <<- eventattrib.scen(...,scenario=baseline)
  invisible(list(sidx=sidx,bidx=bidx))
}
