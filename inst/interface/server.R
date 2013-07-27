library(shiny)
library(indexCurveUncert)
library(ggplot2)

shinyServer(function(input, output, session) {

    getPrefConstraints <- getPrefConstraintsLists
    getWeightConstraints <- getWeightConstraintsLists

    if(exists("index.all.default")) index.all <- index.all.default
    else index.all <- list()
    if(exists("pref.bounds.default")) pref.bounds <- pref.bounds.default
    else pref.bounds <- list()
    if(exists("pref.monoton.default")) pref.monoton <- pref.monoton.default
    else pref.monoton <- list()
    ## TODO: should generate automatically?
    if(exists("attribs.usesduration.default")) attribs.usesduration <- attribs.usesduration.default
    else attribs.usesduration <- NULL

    ## TODO: use existing
    weight.bounds <- list()
    weight.comp <- list()

    environment(getPrefConstraints) <- environment()
    environment(getWeightConstraints) <- environment()
    environment(envindex.diff.qp) <- environment()
    environment(plot.envindex.bound) <- environment()
    environment(plot.unique.prefs) <- environment()
    environment(vary_all) <- environment()
    environment(showPrefConstraintsLists) <- environment()

    ##TODO: trigger observe when document onready
    ##browser()
    specieslist<-unique(gsub("(.*)_.*.csv","\\1",names(index.all)))
    choices <-  1:nrow(asset.table)
    names(choices) <- as.character(asset.table$Name)
    updateCheckboxGroupInput(session,"ctf_assets",choices=as.list(choices),selected=NULL)
    updateRadioButtons(session,"assets_shown",choices=as.list(choices),selected=NULL)

    updateRadioButtons(session,"attrib_shown",choices=names(attribs.usesduration))
    updateRadioButtons(session,"species_shown",choices=specieslist)
    updateCheckboxGroupInput(session,"ctf_species",choices=specieslist)
    updateCheckboxGroupInput(session,"delete_species",choices=specieslist)
    updateCheckboxGroupInput(session,"attribs",choices=names(attribs.usesduration))
    updateSelectInput(session,"scenario",choices=names(all.hydroinputlist),selected=names(all.hydroinputlist)[1])
    updateSelectInput(session,"baseline",choices=names(all.hydroinputlist),selected=names(all.hydroinputlist)[2])
    cat("Finished setting values\n")

    observe({
        ready.structure()
        specieslist<<-unique(gsub("(.*)_.*.csv","\\1",names(index.all)))
        choices <-  1:nrow(asset.table)
        names(choices) <- as.character(asset.table$Name)
        updateCheckboxGroupInput(session,"ctf_assets",choices=as.list(choices),
                                 selected=intersect(isolate(input$ctf_assets),choices))
        updateRadioButtons(session,"assets_shown",choices=as.list(choices),
                           selected=names(choices)[as.numeric(isolate(input$assets_shown))])

        updateRadioButtons(session,"attrib_shown",choices=names(attribs.usesduration),
                           selected=isolate(input$attrib_shown)
                           )
        updateRadioButtons(session,"species_shown",choices=specieslist,
                           selected=isolate(input$species_shown)
                           )
        updateCheckboxGroupInput(session,"ctf_species",choices=specieslist,
                                 selected=intersect(isolate(input$ctf_species),specieslist)
                                 )
        updateCheckboxGroupInput(session,"attribs",choices=names(attribs.usesduration),
                                 selected=intersect(isolate(input$attribs),names(attribs.usesduration))
                                 )
    })


################################################################################
    ## Saving and loading settings

    ready.structure <- reactive({
        cat("ready.structure updated\n",file=stderr())
        handle.uploaded()
        update.bkpts()
        update.species()
        TRUE
    })

    handle.uploaded <- reactive({
        cat("Handle upload\n")
        inFile <- input$file1

        if (!is.null(inFile)){
            cat(sprintf(" Loading %s\n",inFile$datapath))
            e <- new.env()
            load(inFile$datapath,envir=e)
            pref.bounds <<- e$pref.bounds
            pref.monoton <<- e$pref.monoton
            weight.bounds <<- e$weight.bounds
            weight.comp <<- e$weight.comp
            index.all <<- e$index.all
        }
    })

    output$download_prefs <-
        downloadHandler(
                        filename="indexCurveSettings.Rdata",
                        content=function(file){
                            save(pref.bounds,
                                 pref.monoton,
                                 weight.bounds,
                                 weight.comp,
                                 index.all,
                                 file=file)
                        }
                        )


################################################################################
    ## Updating prefs:
    ## When wpref changes, need to set value first
    ## Need to still set even if null
    ## Shouldn't run xx between change in wpref and upload
    ## TODO: problem with ordering of operations
    ## Then can update

    ## first time it downloads after uploading it gets wrong
    ## resizing triggers download? - disabled?

    wpref <- reactive({
        cat("Change attrib/species\n")
        wpref1 <- sprintf("%s_%s.csv",
                         input$species_shown,
                         input$attrib_shown)
        wpref1
    })

    observe({
      cat("Set ctf\n")
      if(length(input$assets_shown)>0 & input$assets_shown %in% 1:nrow(asset.table)){
        updateNumericInput(session,"ctf_shown",
                           value=asset.table$Event_threshold[as.numeric(input$assets_shown)])
        just.set.ctf <<- TRUE
      }
    })

    observe({
        input$btn_discard_pref
        ready.structure()
        wpref1 <- wpref()
        ## update slider
        ## if (wpref1 %in% names(index.all)) updateSliderInput(session,"range",
        ##                                                     value=max(index.all[[wpref1]][,1]))
        if (wpref1 %in% names(index.all)) {
            updateNumericInput(session,"range",
                               value=max(index.all[[wpref1]][,1]))
            ##TODO: avoid duplicating
            updateNumericInput(session,"range2",
                               value=max(index.all[[wpref1]][,1]))
            ## pref.bounds
            cat("Upload prefs\n",file=stderr())
            df <- pref.bounds[[wpref1]]
            if(!is.null(df)){
                ## TODO: more elegant solution to ensuring returns array of arrays?
                if(nrow(df)==1) df <- rbind(df,c(NA,NA,NA,NA))
                dfa=as.list(df)
                names(dfa)=NULL
            } else {
                dfa <- list(list(NA_real_),list(NA_real_),list(NA_real_),list(NA_real_))
            }
            ##browser()
            session$sendInputMessage("pref_bounds",list(data=dfa))
            ## pref.monoton
            df <- pref.monoton[[wpref1]]
            if(!is.null(df)){
                if(nrow(df)==1) df <- rbind(df,c(NA,NA,NA,NA))
                dfa=as.list(df)
                names(dfa)=NULL
            } else dfa <- list(list(NA_real_),list(NA_real_),list(NA_real_),list(NA_real_))
            session$sendInputMessage("pref_monoton",list(data=dfa))
        }
        ## return current wpref
        wpref1
    })

    ## input.pref.bounds <- reactive({
    ##     wpref()
    ##     input$pref_bounds
    ## })
    ## input.pref.monoton <- reactive({
    ##     wpref()
    ##     input$pref_monoton
    ## })


    ## Only changes with btn_update_pref
    ## FIXME: saving without changes after discarding actually saves discarded changes. But looks like repeated discard or new changes save correctly.
    update.prefs <- reactive({
        input$btn_update_pref
        wpref1 <- isolate(wpref())
        ## pref.bounds
        cat("Download bounds\n",file=stderr())
        pref.bounds[[wpref1]] <<- as.data.frame(isolate(input$pref_bounds))
        ##pref.bounds[[wpref1]] <<- as.data.frame(input.pref.bounds())
        names(pref.bounds[[wpref1]]) <<- c("min.x","max.x","min.y","max.y")
        pref.bounds[[wpref1]] <<- pref.bounds[[wpref1]][!apply(pref.bounds[[wpref1]],1,function(x) all(is.na(x))),,drop=FALSE]
        if(nrow(pref.bounds[[wpref1]])==0) pref.bounds[[wpref1]] <<- NULL
        ##browser() ## see what pref.bounds ends up as

        ## pref.monoton
        ##pref.monoton[[wpref1]] <<- as.data.frame(input.pref.monoton())
        pref.monoton[[wpref1]] <<- as.data.frame(isolate(input$pref_monoton))
        names(pref.monoton[[wpref1]]) <<- c("min.x","max.x","dir","min.step")
        pref.monoton[[wpref1]] <<- pref.monoton[[wpref1]][!apply(pref.monoton[[wpref1]],1,function(x) all(is.na(x))),,drop=FALSE]
        if(nrow(pref.monoton[[wpref1]])==0) pref.monoton[[wpref1]] <<- NULL
    })


    output$pref_title <- renderText(sprintf("Constraints on suitability: %s for %s",input$attrib_shown,input$species_shown))

################################################################################

    checkOkToRun <- reactive({
        return(input$scenario %in% names(all.hydroinputlist) &&
               input$baseline %in% names(all.hydroinputlist) &&
               (input$species_shown %in% specieslist || all(input$species %in% specieslist)) &&
               ##TODO: probably better to pass as argument to a function
               (input$attrib_shown %in% names(attribs.usesduration) || all(input$attribs %in% names(attribs.usesduration)))
               )
    })

    ## depends on species,asset,ctf,use_duration,attrib,update.prefs
    xx <- reactive({
        ready.structure()
        update.prefs()
        cat("envindex.diff.qp\n",file=stderr())
        ##index.all$test_dry.csv<<-input$bkpts_dry
        ##cat(input$use_duration,"\n")
        ##cat(str(getPrefConstraints("test","dry"),file=stderr()))
        ## browser() ## see what environment envindex.diff.qp is working in
        input$scenario
        input$baseline
        input$species_shown
        input$assets_shown
        input$ctf_shown
        input$use_duration
        input$attrib_shown
        if(checkOkToRun()){
            envindex.diff.qp(scen=input$scenario,baseline=input$baseline,
                             ecospecies=input$species_shown,
                             assetid=as.numeric(input$assets_shown),
                             ctf=as.numeric(input$ctf_shown),
                             use.durs=as.logical(input$use_duration),
                             attribs.usesduration = attribs.usesduration[input$attrib_shown]
                             )
        }
    })

    ##reactive(cat(print(xx()),file=stderr()))

    ## depends on xx,attrib_shown or range
    output$pref <- renderPlot({
        ## par(mfrow=c(length(input$attribs),length(input$species)))
        ## plot(xx())
        ##cat(input$attrib_shown,"\n")
        ##plot.unique.prefs(xx(),attrib=input$attrib_shown)
##############
        ## TODO: adjust ncol
        diffs <- xx()
        cat("Preference plot\n")
        attrib <- isolate(input$attrib_shown)
        del <- duplicated(lapply(diffs, function(x) do.call(c, x[sprintf("pars.%s.%s", c("max", "min"), attrib)])))
        diffs <- diffs[!del]
        ncol=min(4,NROW(diffs))
        #3par(mfrow = c(ceiling(NROW(diffs)/ncol), ncol))
        ##par(mfrow = c(ceiling((NROW(diffs))/ncol), ncol))
        ##browser()
        if(!is.null(diffs)){
            if(NROW(diffs)==1) layout(t(1:2), widths=c(7,3), heights=1)
            if(NROW(diffs)==2) layout(t(1:3), widths=c(35,35,30), heights=1)
            ##
            plot(diffs, attribs = input$attrib_shown,
                 xlim=c(0,input$range),main=""
                 )
            ###
            uu <- do.call(rbind,lapply(diffs,function(x) data.frame(x[c("diff.min","diff.max")])))
            uu$id <- 1:length(diffs)
            ##uu$id <- factor(1:length(diffs))
            ##http://monkeysuncle.stanford.edu/?p=485
            plot(NA,xlim=c(0,nrow(uu)+1),ylim=range(c(uu$diff.min,uu$diff.max)),xlab="",
                 ylab=sprintf("%s better -- %s better",input$baseline,input$scenario))
            rect(xleft=-100, ybottom=0, xright=100, ytop=100,col=green(),border=NA)
            rect(xleft=-100, ybottom=-100, xright=100, ytop=0,col=red(),border=NA)
            for(i in 1:nrow(uu)) arrows(uu$id[i],uu$diff.min[i], uu$id[i], uu$diff.max[i], angle=90, code=3, length=0.1)
            ##abline(h=0,lty=2)
            ##ggplot(data=uu)+geom_errorbar(aes(x=id,ymin=diff.min,ymax=diff.max))+xlab("")+scale_x_discrete(labels="")
        } else { NULL }
    })


    output$pref_text <- renderPrint({
        ##TODO: make numbers optional
        showPrefConstraintsLists(input$species_shown,input$attrib_shown)
        ##print(xx())
    })
    ##FIXME: Unsupported type
    ## output$pref_text <- renderTable({
    ##     ##print(xx())
    ##     do.call(rbind, lapply(xx(), function(r) r[c("assetid",
    ##     "ctf", "species", "diff.min", "diff.max", "use.dur")]))
    ## })


################################################################################
    ## Weights

    output$weight_title <- reactive({
        sprintf("Weight constraints for species %s",input$species_shown)
    })

    as.arrays <- function(df,n=4){
        ##browser()
        if(!is.null(df)){
            ## TODO: more elegant solution to ensuring returns array of arrays?
            if(nrow(df)==1) df <- rbind(df,rep(NA,ncol(df)))
            dfa=as.list(df)
            names(dfa)=NULL
        } else {
            dfa <- rep(list(list(NA_real_)),n)
        }
        dfa
    }


    observe({
        input$btn_discard_weights
        ready.structure()
        cat("Upload weights\n",file=stderr())
        if(!is.null(input$species_shown)){
            wb <- weight.bounds[[input$species_shown]]
            wc <- weight.comp[[input$species_shown]]
        } else { wb <- wc <- NULL}
        if(is.null(wb) | NROW(wb)==0) wb <- data.frame(attrib=NA,min.weight=NA,max.weight=NA,stringsAsFactors=FALSE)
        if(is.null(wc) | NROW(wc)==0) wc <- data.frame(attrib1=NA,attrib2=NA,dir=NA,min.gap=NA,stringsAsFactors=FALSE)
        session$sendInputMessage("weight_bounds",list(data=as.arrays(wb)))
        session$sendInputMessage("weight_comp",list(data=as.arrays(wc)))
    })

    ## Only changes with btn_update_weight
    ## FIXME: Not whole matrix of weight_bounds updating at a time.
    update.weights <- reactive({
        input$btn_update_weights
        species <- isolate(input$species_shown)
        in.weight.bounds <- isolate(input$weight_bounds)
        in.weight.comp <- isolate(input$weight_comp)
        if(!is.null(species) && species %in% specieslist){
            cat("Download weights\n",file=stderr())
            if(!is.null(in.weight.bounds)){
                weight.bounds[[species]] <<- as.data.frame(in.weight.bounds,stringsAsFactors=FALSE)
                names(weight.bounds[[species]]) <<- c("attrib","min.weight","max.weight")
                weight.bounds[[species]]$min.weight <<- as.numeric(weight.bounds[[species]]$min.weight)
                weight.bounds[[species]]$max.weight <<- as.numeric(weight.bounds[[species]]$max.weight)
                weight.bounds[[species]]$attrib[weight.bounds[[species]]$attrib=="NA"] <<- NA
                weight.bounds[[species]]$attrib[weight.bounds[[species]]$attrib==""] <<- NA
                weight.bounds[[species]] <<- weight.bounds[[species]][!apply(weight.bounds[[species]],1,function(x) all(is.na(x))),,drop=FALSE]
                if(nrow(weight.bounds[[species]])==0) weight.bounds[[species]] <<- NULL
            }
            if(!is.null(in.weight.comp)){
                weight.comp[[species]] <<- as.data.frame(in.weight.comp,stringsAsFactors=FALSE)
                names(weight.comp[[species]]) <<- c("attrib1","attrib2","dir","min.gap")
                weight.comp[[species]]$min.gap <<- as.numeric(weight.comp[[species]]$min.gap)
                weight.comp[[species]]$dir[weight.comp[[species]]$dir=="NA"] <<- NA
                weight.comp[[species]]$dir[weight.comp[[species]]$dir==""] <<- NA
                weight.comp[[species]]$attrib1[weight.comp[[species]]$attrib1=="NA"] <<- NA
                weight.comp[[species]]$attrib1[weight.comp[[species]]$attrib1==""] <<- NA
                weight.comp[[species]]$attrib2[weight.comp[[species]]$attrib2=="NA"] <<- NA
                weight.comp[[species]]$attrib2[weight.comp[[species]]$attrib2==""] <<- NA
                ##weight.comp[[species]] <<- weight.comp[[species]][!sapply(1:nrow(weight.comp[[species]]),function(i) all(is.na(x))),,drop=FALSE]
                weight.comp[[species]] <<- weight.comp[[species]][!apply(weight.comp[[species]],1,function(x) all(is.na(x))),,drop=FALSE]
                if(nrow(weight.comp[[species]])==0) weight.comp[[species]] <<- NULL
            }
            ##browser() ## see what weight.bounds and weight.comp ends up as
        }
    })


    xx.weights <- reactive({
        ##update.prefs()
        update.weights()
        ready.structure()
        cat("envindex.diff.qp weights\n",file=stderr())
        ##cat(input$use_duration,"\n")
        ##cat(str(getPrefConstraints("test","dry"),file=stderr()))
        ## browser() ## see what environment envindex.diff.qp is working in
        input$scenario
        input$baseline
        input$species_shown
        input$assets_shown
        input$ctf_shown
        input$use_duration
        input$attribs
        if(checkOkToRun()){
            envindex.diff.qp(scen=input$scenario,baseline=input$baseline,
                             ecospecies=input$species_shown,
                             assetid=as.numeric(input$assets_shown),
                             ctf=as.numeric(input$ctf_shown),
                             use.durs=as.logical(input$use_duration),
                             attribs.usesduration = attribs.usesduration[input$attribs]
                             )
        } else{return(NULL)}
    })

    weight.setting <- reactive({
        sprintf("%s_%s_%s_%s_%s",
                input$assets_shown,
                input$ctf_shown,
                input$species_shown,
                paste(input$attribs,collapse="_"),
                paste(input$use_duration,collapse="_")
                )
    })

    output$weights <- renderTable({
        xx.weights()
        ##TODO: duplicate checking?
        if (!is.null(xx.weights()) && input$species_shown %in% specieslist) {
            ##browser() ##constraints used in checking weights
            what.weights(xx.weights())
        } else{
            NULL
        }
    })

    ## TODO: should only use local variables to be concurrency-safe
    ## TODO: allow selecting which run & min or max weights
    ## TODO: need isValid function
    ## FIXME: error length(pp) == idx$ndays is not TRUE
    output$plot_weight_classes <- renderPlot({
        input$current_weight_run
        input$which_weight_run
        weight.setting()
        ready.structure()
        input$scenario
        input$baseline
        input$assets_shown
        input$ctf_shown
        if(checkOkToRun()){
            envindex.diff.getdata(scen=input$scenario,baseline=input$baseline,
                                  assetid=as.numeric(input$assets_shown),
                                  ctf=as.numeric(input$ctf_shown)
                                  )
        }
        sidx <- NULL
        bidx <- NULL
        if(input$species_shown %in% specieslist && weight.setting()==input$current_weight_run){
            wl <- strsplit(input$which_weight_run,"_")[[1]]
            cat(wl,file=stderr())
            rr <- run.scen(xx.weights()[[as.numeric(wl[2])]],dir=wl[1],attribs.usesduration[input$attribs])
            plot.weight.classes((sapply(rr$pp.s,mean)-sapply(rr$pp.b,mean))*100, ##sapply(rr$pp.s,sum)-sapply(rr$pp.b,sum),
                                current.weights=xx.weights()[[as.numeric(wl[2])]][[sprintf("pars.%s.weights", wl[1])]])
        } else { NULL }
    })

    observe({
        xx.weights()
        choices <- apply(expand.grid(c("min","max"),1:length(xx.weights())),1,paste,collapse="_")
        names(choices) <- unlist(lapply(xx.weights(),function(r) sprintf("%d_%d_%s_%s_%s",r$assetid,r$ctf,r$species,r$use.dur,c("min","max"))))
        updateSelectInput(session,"which_weight_run",
                          choices=choices,select=names(choices)[1])
        updateTextInput(session,"current_weight_run",value=weight.setting())
    })

################################################################################
    xx.asset <- reactive({
        update.prefs()
        update.weights()
        ready.structure()
        cat("asset envindex.diff.qp\n",file=stderr())
        ##cat(input$use_duration,"\n")
        ##cat(str(getPrefConstraints("test","dry"),file=stderr()))
        ## browser() ## see what environment envindex.diff.qp is working in
        input$scenario
        input$baseline
        input$species_shown
        input$ctf_shown
        input$use_duration
        input$attribs
        input$assets_shown
        if(checkOkToRun()){
            envindex.diff.qp(scen=input$scenario,baseline=input$baseline,
                             ecospecies=input$species_shown,
                             assetid=as.numeric(input$assets_shown),ctf=as.numeric(input$ctf_shown),
                             use.durs=as.logical(input$use_duration),
                             attribs.usesduration = attribs.usesduration[input$attribs]
                             )
        } else{return(NULL)}
    })

    xx.noctf <- reactive({
        update.prefs()
        update.weights()
        ready.structure()
        species <- input$ctf_species
        ##print(species)
        if(checkOkToRun() && !is.null(species)){
            cat("all.diffs noctf\n",file=stderr())
            ##browser()
            all.diffs <- vary_all(scen=input$scenario,baseline=input$baseline,
                                  ecospecies=species,
                                  use.durs=as.logical(input$use_duration),
                                  attribs.usesduration=attribs.usesduration[input$attribs],
                                  assets=as.numeric(input$ctf_assets),
                                  getSeqCtfs=function(assetid) 1,
                                  do.par=F ##FIXME
                                  )
        } else{return(NULL)}
    })

    ## depends on xx,attrib_shown or range
    output$pref_asset <- renderPlot({
        ## par(mfrow=c(length(input$attribs),length(input$species)))
        ## plot(xx())
        ##cat(input$attrib_shown,"\n")
        ##plot.unique.prefs(xx(),attrib=input$attrib_shown)
##############
        ## TODO: adjust ncol
        ##diffs <- xx.asset()
        diffs <- xx.noctf()
        cat("Preference plot\n")
        attrib <- isolate(input$attribs)
        del <- duplicated(lapply(diffs, function(x) do.call(c, x[sprintf("pars.%s.%s", c("max", "min"), attrib)])))
        diffs <- diffs[!del]
        ##par(mfrow = c(ceiling((NROW(diffs))/ncol), ncol))
        ##browser()
        if(!is.null(diffs)){
            ##browser()
            layout(matrix(1:(2*length(diffs)),ncol=2), widths=c(7,3), heights=rep(1,length(diffs)))
            ##
            plot(diffs, attribs = attrib,
                 ##TODO: range
                 xlim=c(0,50),main=""
                 )
            ###
            uu <- do.call(rbind,lapply(diffs,function(x) data.frame(x[c("diff.min","diff.max")])))
            uu$id <- 1:length(diffs)
            ##uu$id <- factor(1:length(diffs))
            ##http://monkeysuncle.stanford.edu/?p=485
            for(i in 1:nrow(uu)){
                plot(NA,xlim=c(0,2),ylim=c(uu$diff.min[i],uu$diff.max[i]),xlab="",
                     ylab=sprintf("%s better -- %s better",input$baseline,input$scenario))
                rect(xleft=-100, ybottom=0, xright=100, ytop=100,col=green())
                rect(xleft=-100, ybottom=-100, xright=100, ytop=0,col=red())
                arrows(1,uu$diff.min[i], 1, uu$diff.max[i], angle=90, code=3, length=0.1)
            }
            ##abline(h=0,lty=2)
            ##ggplot(data=uu)+geom_errorbar(aes(x=id,ymin=diff.min,ymax=diff.max))+xlab("")+scale_x_discrete(labels="")
        }
    })

################################################################################
    ## TODO: give indication of progress/delay
    ## TODO: if already run, update existing rather than re-running all
    ## TODO: and/or add action button rather than auto compuete?
    xx.all <- reactive({
        update.prefs()
        update.weights()
        species <- input$ctf_species
        ##print(species)
        if(checkOkToRun() && !is.null(species)){
            cat("all.diffs",file=stderr())
            all.diffs <- vary_all(scen="Post90",baseline="Pre90",
                                  ecospecies=input$ctf_species,
                                  use.durs=as.logical(input$use_duration),
                                  attribs.usesduration=attribs.usesduration[input$attribs],
                                  assets=as.numeric(input$ctf_assets),
                                  getSeqCtfs=getSeqCtfs10,
                                  do.par=F ##FIXME
                                  )
        }
    })


    ## TODO: non-ctf plot with xx.asset
    ## TODO: all.diffs with ctf plot
    output$traffic_ctf <- renderPlot({
        all.diffs <- xx.all()
        ## Extract data.frame
        tab <- as.data.frame(subset(all.diffs,subset=!is.na(diff.min)))
        ## Convert to long form
        tabm <- melt(tab,id.var=c("assetid","ctf","species","use.dur"))
        ## Convert continuous variable to discrete - which scenario is favoured
        tabm$value <- ifelse(tabm$value>0,"Scenario","Baseline or =")

        ## Check for uncertainty
        answers <- marginalise(tabm,assetid+species+ctf~.,verbose=T)
        answers$assetid <- ordered(answers$assetid,level=1:nrow(asset.table),label=asset.table$Name)
        answers$"(all)" <- ordered(answers$"(all)",level=c("Scenario","Uncertain","Baseline or ="))
        ## Traffic light CTF plot
        ##  Answer for each ctf, species & asset with ctf overlain
        pp <- ggplot(data=answers)+
            facet_wrap(~assetid,scale="free")+
                geom_point(aes(y=species,x=ctf,col=get("(all)")),size=4)+
                    scale_color_manual(name="Which scenario\nis better",
                                       values=c(
                                       ##Plus
                                       "Scenario"=green(),
                                       ##Uncertain
                                       "Uncertain"=rgb(241,242,241,maxColorValue=255),
                                       ##Minus
                                       "Baseline or ="=red()
                                       ))+
                                           ylab("Species")+
                                               xlab("Commence-to-flow level (ML/day)")
        ##TODO: ggplot can't set breaks for each panel?
        ##scale_x_continuous(breaks=seq(2000,20000,by=2000))+
        ##geom_vline(aes(xintercept=value),
        ##data=subset(all.ctf,assetid %in% unique(answers$assetid)),linetype="dashed")
        print(pp)
    })

################################################################################

    output$summary <- renderPrint({
        update.prefs()
        update.weights()
        ready.structure()
        print(str(all.hydroinputlist))
        print(xx())
        ##print(what.weights(xx()))
    })
    output$print_weights <- renderPrint({
        update.weights()
        ready.structure()
        cat("weight.bounds\n")
        print(str(weight.bounds))
        cat("weight.comp\n")
        print(str(weight.comp))
    })
    output$print_prefs <- renderPrint({
        update.prefs()
        ready.structure()
        cat("pref.bounds\n")
        cat(str(pref.bounds))
        cat("pref.monoton\n")
        cat(str(pref.monoton))
    })
    output$print_bkpts <- renderPrint({
        ready.structure()
        print(str(index.all))
    })
################################################################################

    getdata <- reactive({
        input$btn_discard_bkpts
        ready.structure()
        cat("envindex.getdata\n",file=stderr())
        if(checkOkToRun()){
            ## browser() ## see what environment envindex.diff.getdata is working in
            list(sidx=eventattrib.scen(scenario=input$scenario,
                 assetid=as.numeric(input$assets_shown),
                 ctf=as.numeric(input$ctf_shown)
                 ),
                 bidx=eventattrib.scen(scenario=input$baseline,
                 assetid=as.numeric(input$assets_shown),
                 ctf=as.numeric(input$ctf_shown)
                 )
                 )
        }
    })

    ##checkAttributeRanges("Pre90",specieslist,attribs=names(attribs.usesduration))
    ##checkAttributeRanges("Post90",specieslist,attribs=names(attribs.usesduration))


    output$breakpoint_plot <- renderPlot({
        ready.structure()
        ee <- getdata()
        cpt <- index.all[[sprintf("%s_%s.csv", input$species_shown,
                                  input$attrib_shown)]]
        input$range2
        if(!is.null(cpt) && !is.null(ee$sidx) && !is.null(ee$bidx)){
            ##browser()
            ## ggplot()+geom_density(aes(x=x),data=data.frame(x=ee$sidx$events[[input$attrib_shown]]))+
            ##     geom_density(aes(x=x),data=data.frame(x=ee$bidx$events[[input$attrib_shown]]))
            print(ggplot(data=rbind(
                         data.frame(x=ee$sidx$events[[input$attrib_shown]],scen="Scenario"),
                         data.frame(x=ee$bidx$events[[input$attrib_shown]],scen="Baseline")
                         ))+
                  geom_vline(aes(xintercept=x),
                             linetype="dashed",color="black",size=1,
                             data=data.frame(x=cpt[,1]))+
                  geom_density(aes(x=x,color=scen))+
                  geom_point(aes(x = x,y=0,color=scen), size = 2, position=position_jitter(w = 0.1, h = 0.005))+
                  scale_x_continuous(name=capwords(input$attrib_shown),limits=c(0,input$range2))+
                  scale_colour_manual(values = c("Scenario" = green(),"Baseline"=red()))
                  )
           ## plot(NULL,ylim=c(0,1),xlim=c(0,input$range2),
           ##      xlab=capwords(input$attrib_shown),ylab="Suitability index")
           ##abline(v = cpt[, 1], lty = "dashed", col = "grey")
       }
    })

    observe({
        input$btn_discard_bkpts
        ready.structure()
        wpref1 <- wpref()
        if(wpref1 %in% names(index.all)){
            cat("Upload breakpoints\n",file=stderr())
            df <- index.all[[wpref1]]
            if(is.data.frame(df) && ncol(df)>1) df <- df[,1,drop=FALSE]
            session$sendInputMessage("matrix_bkpts",list(data=as.arrays(df,1)))
        }
        ## return current wpref
        wpref1
    })

    ## Only changes with btn_update_bkpts
    ## FIXME: race condition / failure to take - matrix_bkpts out of date. Clicking discard to redo upload seems to correct problem
    update.bkpts <- reactive({
        input$btn_update_bkpts
        wpref1 <- isolate(wpref())
        if(wpref1 %in% names(index.all)){
            cat("Download bkpts\n",file=stderr())
            ##browser()
            ##TODO: don't discard other information, like names?
            index.all[[wpref1]] <<- as.data.frame(isolate(input$matrix_bkpts))
            index.all[[wpref1]] <<- index.all[[wpref1]][!apply(index.all[[wpref1]],1,function(x) all(is.na(x))),,drop=FALSE]
            if(nrow(index.all[[wpref1]])==0) index.all[[wpref1]] <<- NULL
            if(!is.null(index.all[[wpref1]])) index.all[[wpref1]] <<- index.all[[wpref1]][order(index.all[[wpref1]][,1]),,drop=FALSE]
            ##browser() ## see what ends up as
            ## #############
            ## Add sequence
            from=as.numeric(isolate(input$bkpt_from))
            to=as.numeric(isolate(input$bkpt_to))
            by=as.numeric(isolate(input$bkpt_by))
            cat(from,to,by,"\n",file=stderr())
            if(!is.na(from) && !is.na(to) && !is.na(by) && by>0 && from<to) index.all[[wpref1]] <<-
                data.frame(V1=unique(c(index.all[[wpref1]][,1],seq(from,to,by=by))))
            if(!is.null(index.all[[wpref1]])) index.all[[wpref1]] <<- index.all[[wpref1]][order(index.all[[wpref1]][,1]),,drop=FALSE]
        }
        TRUE
    })

################################################################################

    observe({
        ready.structure()
        input$btn_discard_species
        updateCheckboxGroupInput(session,"delete_species",choices=specieslist,selected=NULL)
    })

    ## Only changes with btn_update_species
    update.species <- reactive({
        input$btn_update_species
        delete_species <- isolate(input$delete_species)
        add_species <- isolate(input$add_species)
        ##browser()
        if(add_species=="") add_species <- NA
        cat("Update species\n",file=stderr())
        for(s in delete_species){
            cat("Deleting ",paste(grep(s,names(index.all),value=T),collapse=", "),"\n",file=stderr())
            index.all[grep(s,names(index.all))] <<- NULL
        }
        ##FIXME: avoid clobbering if already exists
        if(!is.na(add_species) && ! add_species %in% specieslist) {
            for(aa in names(attribs.usesduration)) {
                cat("Adding dummy ",sprintf("%s_%s.csv",add_species,aa),"\n",file=stderr())
                index.all[[sprintf("%s_%s.csv",add_species,aa)]] <<- data.frame(0)
            }
        }
    })

################################################################################
    ##data.frame(ID=NA,Gauge=NA,Name=NA,Event_threshold=NA)
    observe({
        ready.structure()
        input$btn_discard_assets
        df <- subset(asset.table,select=c(ID,Gauge,Name,Event_threshold))
        session$sendInputMessage("matrix_assets",list(data=as.arrays(df,4)))
    })

################################################################################
    observe({
        ready.structure()
        input$btn_discard_attribs
        df <- data.frame(Attribute=names(attribs.usesduration),Duration=attribs.usesduration)
        session$sendInputMessage("matrix_attribs",list(data=as.arrays(df,2)))
    })
})
