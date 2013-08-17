eventattrib.scen <-
function (scenario, assetid, ctf, gap = 2, mindur = 3, cache.attribs = FALSE)
{
    ## If attribute is in the cache, return it
    ##TODO: generalise to allow ..., so that ctf is not required
    scen.txt <- ifelse(is.character(scenario),scenario,digest(scenario))
    assetid.txt <- ifelse(is.null(assetid),-99,assetid)
    cache.id <- paste(scen.txt, assetid.txt, ctf, sep = " ")
    if (cache.attribs && !exists("attrib.cache"))
        attrib.cache <<- list()
    if (cache.attribs)
        cached <- attrib.cache[[cache.id]]

    if (cache.attribs && !is.null(cached)) {
        return(cached)
    } else if (!cache.attribs | (cache.attribs && is.null(cached))) {
        ## If list attrib.fns is defined, use it
        ## TODO: convert existing functions to be used in attrib.fns
        ## TODO: need pre-processing functionality or just let the user do that?
        if(exists("attrib.fns")){
            res <- list(assetid = assetid, scenario = scenario, ctf = ctf,
                        ndays = attrib.fns$ndays(scenario,assetid,ctf,gap,mindur),
                        events=list()
                        )
            for(f in names(attrib.fns)){
                if(f!="ndays"){
                    res$events[[f]] <- attrib.fns[[f]](scenario,assetid,ctf,gap,mindur)
                }
            }
            if (cache.attribs)
                attrib.cache[[cache.id]] <<- res
            return(res)
        }

        ## Obtain required time series
        if (is.character(scenario)) {
            ## Streamflow. Might be NULL. Hardcoded first column
            gauge <- asset.table[assetid, 2]
            gaugez <- all.hydroinputlist[[scenario]][[paste(gauge,".csv", sep = "")]]
            surfaceflow <- gaugez[, 1]
            if(!is.null(gaugez)) {
                baseflow <- gaugez$Baseflow
            } else {baseflow <- NULL}
            ## Groundwater level. Might be NULL
            bore <- asset.table$Bore[assetid]
            gwlevel <- all.hydroinputlist[[scenario]]$gwlevel.csv[,as.character(bore)]
            ## Combine to ensure same length and same NAs
            combined <- list(gwlevel = gwlevel, surfaceflow = surfaceflow,baseflow=baseflow, all = F)
            combined <- do.call(cbind.zoo, combined[!sapply(combined,
                                                            is.null)])
            ## Place Nas in all non-complete rows.
            ## Later omitted for gw,and don't appear
            coredata(combined)[!complete.cases(combined), ] <- NA
            if (!is.null(gwlevel) && !is.null(surfaceflow) && !identical(index(gwlevel), index(surfaceflow))) {
                warning(sprintf("In %s with assetid %d gwlevel and surfaceflow time periods do not match, automatically trimming\nNew time range %s to %s",
                                scenario, assetid, min(index(combined)), max(index(combined))))
            }
            surfaceflow <- combined$surfaceflow
            baseflow <- combined$baseflow
            gwlevel <- combined$gwlevel
        } else if (is.zoo(scenario)) {
            surfaceflow <- scenario
            baseflow <- NULL
            gwlevel <- NULL
        } else if (is.list(scenario)) {
            surfaceflow <- scenario$surfaceflow
            baseflow <- scenario$baseflow
            gwlevel <- scenario$gwlevel
            if (is.null(assetid))
                assetid <- -99
            ## Fail if don't have same time indices
            stopifnot(is.null(gwlevel) | identical(index(gwlevel),
                                                   index(surfaceflow)))
        } else {
            stop("Object has unexpected class")
        }
        ## Return attributes from time series
        if(!is.null(surfaceflow)) {
            flowevent <- eventseq(surfaceflow, thresh = ctf, mingap = gap,
                                  mindur = mindur)
            flowevent.attrib <- eventattrib(surfaceflow, flowevent,
                                            FUN = mean)
        } else {
            flowevent.attrib=list()
        }
        gwlevel <- coredata(gwlevel)
        gwlevel <- na.omit(gwlevel)
        res <- list(assetid = assetid, scenario = scenario, ctf = ctf,
                    ndays = NROW(combined), ##length(na.omit(coredata(surfaceflow))),
                    events = c(flowevent.attrib$ddd,list(gwlevel = gwlevel)))
        if (cache.attribs)
            attrib.cache[[cache.id]] <<- res
        return(res)

    } else {
        stop("Unexpected cache.attribs state")
    }
}
