## Interactively add preference constraints starting from a blank slate
## Combined with initial indices and with caching

## 1. Load data
library(indexCurveUncert)
loadHydro("X:/phd/papers/namoi ua/Namoi model/Inputs EMS paper/",c("Pre90","Post90"))
loadAssets("X:/phd/papers/namoi ua/Namoi model/Inputs EMS paper/ctf_v4.csv")

## 2. Specify break-point x values & load existing indices
## Multiple indices are merged
loadIndexes("C:\\temp\\My Dropbox\\temp\\index_all")
loadIndexes("X:/phd/papers/namoi ua/Namoi model/Inputs EMS paper/index/")

specieslist <- c("RRGMS", "RRGRR", "BBMS", "BBRR", "LGMS", "LGRR", "WCMS", "WCRR")

################################################################################
## 3. Extend  indices TODO: generalise
##indexes[grep("dry",names(indexes))]
## TODO: can't be Inf, need to be finite breakpoint, but needs to be big enough. Value determines slope, if any
max.dry <- 25000
for(s in c("RRGMS","BBMS","LGMS","WCMS","WCRR")){
    cpt <- index.all[[sprintf("%s_%s.csv", s,"dry")]]
    index.all[[sprintf("%s_%s.csv", s,"dry")]] <-
        rbind(cpt,
              c(Days=max.dry,rep(0,ncol(cpt)-1)))
}
for(s in c("RRGRR","BBRR","LGRR")){
    cpt <- index.all[[sprintf("%s_%s.csv", s,"dry")]]
    index.all[[sprintf("%s_%s.csv", s,"dry")]] <-
        rbind(cpt,
              c(Days=max.dry,rep(0.5,ncol(cpt)-1)))
}


##Duration
for(s in c("RRGMS", "RRGRR", "BBMS", "BBRR", "LGMS", "LGRR"))
    index.all[[sprintf("%s_%s.csv", s,"duration")]] <-
        rbind(index.all[[sprintf("%s_%s.csv", s,"duration")]],
              c(Days=1000,rep(NA,5)))

## Gwlevel
## index.all[grep("gwlevel",names(index.all),value=T)]
for(s in specieslist)
    index.all[[sprintf("%s_%s.csv", s,"gwlevel")]] <-
        rbind(index.all[[sprintf("%s_%s.csv", s,"gwlevel")]],
              c(Level_m=30,0))

################################################################################
## 4. Cache mult-index preferences
## TODO: don't clobber existing index.all and getPrefConstraints
getPrefConstraints <- getPrefConstraintsMultIndex
cachePreferences(grep("test",names(index.all),invert=T,value=T)) ##not for test

################################################################################
## 5. Set pref constraints method
## Get constraints from cache.
## Use cache and add extra constraints from lists
## TODO: cache list prefs instead of merging
getPrefConstraints <- function(...)
  getPrefConstraintsMergeWithCache(...,extra=getPrefConstraintsLists)

################################################################################
## 6. Interactive/override constraints
################################################################################
## i. Reset gwlevel breakpoints and constraints
## Add additional breakpoints to effectively allow variation in x values of breakpoints
index.all$RRGMS_gwlevel.csv <- merge(index.all$RRGMS_gwlevel.csv,
                                     data.frame(X1=c(seq(0,15,length.out=20),20)),
                                     all=T,by=1
                                     )
index.all$RRGMS_gwlevel.csv
                                     
## Delete existing constraints for gwlevel (as they result in single line)
cached.pref$RRGMS_gwlevel.csv <- NULL
## Check that gwlevel has no constraints being used
getPrefConstraints("RRGMS","gwlevel")

## ii.
##min.x,max.x,min.y,max.y all inclusive
pref.bounds <- list()
## No gw access beyond 12m
pref.bounds$RRGMS_gwlevel.csv <- data.frame(min.x=12,max.x=Inf,min.y=NA,max.y=0)
## GW access good between 2 and 5 m
pref.bounds$RRGMS_gwlevel.csv <- rbind(pref.bounds$RRGMS_gwlevel.csv,
                                      data.frame(min.x=2,max.x=5,min.y=1,max.y=1))

## iii.
## Monotonicity constraints
##min.x,max.x,dir -> f(min.x)<=f(max.x)
pref.monoton <- list()
pref.monoton$RRGMS_gwlevel.csv <- data.frame(min.x=5,max.x=12,dir=-1,min.step=0)


################################################################################
## 7. Vary all and run

attribs.usesduration = c(timing = "duration", duration = "duration", 
                         dry = "duration",gwlevel=NA)


## CTF values to be used are returned by getSeqCtfs function
##lapply(1:nrow(asset.table),getSeqCtfs)

all.diffs <- vary_all(scen="Post90",baseline="Pre90",ecospecies=specieslist,
                      use.durs=c(T,F),
                      attribs.usesduration=attribs.usesduration,
                      assets=1:nrow(asset.table),getSeqCtfs=getSeqCtfs10
                      )
##124 sec

##save(all.diffs,file="vary_all_qp.Rdata")


################################################################################
## Visualisation data preparation
##load("vary_all_qp.Rdata")

library(reshape)
library(ggplot2)

## Extract data.frame
tab <- as.data.frame(subset(all.diffs,subset=!is.na(diff.min)))
## Convert to long form
tabm <- melt(tab,id.var=c("assetid","ctf","species","use.dur"))
## Convert continuous variable to discrete - which scenario is favoured
tabm$value <- ifelse(tabm$value>0,"Scenario","Baseline or =")

## Check for uncertainty
answers <- marginalise(tabm,assetid+species+ctf~.,verbose=T)

## Make variables more readable, and ordered
answers$assetid <- ordered(answers$assetid,level=1:nrow(asset.table),label=asset.table$Name)
answers$"(all)" <- ordered(answers$"(all)",level=c("Scenario","Uncertain","Baseline or ="))
lookup <- c("RRGMS"="River red\ngum MS",
            "RRGRR"="River red\ngum RR",
            "BBMS"="Black box\nMS",
            "BBRR"="Black box\nRR",
            "LGMS"="Lignum\nMS",
            "LGRR"="Lignum\nRR",
            "WCMS"="Water couch\nMS",
            "WCRR"="Water couch\nRR"
            )
answers$species <- lookup[as.character(answers$species)]

################################################################################
## Convert CTF levels from asset.table for comparison
all.ctf <- melt(asset.table[,3:6])
names(all.ctf) <- c("assetid","variable","value")

## Traffic light CTF plot
##  Answer for each ctf, species & asset with ctf overlain
ggplot(data=answers)+
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
    xlab("Commence-to-flow level (ML/day)")+
    ##TODO: ggplot can't set breaks for each panel?
    ##scale_x_continuous(breaks=seq(2000,20000,by=2000))+
    geom_vline(aes(xintercept=value),
               data=subset(all.ctf,assetid %in% unique(answers$assetid)),linetype="dashed")

##ggsave("traffic_viz_vary_all.wmf",width=7.5,height=3)
##ggsave("traffic_viz_vary_all.wmf")

################################################################################
## Explore single point
eg <- subset(all.diffs,assetid==1 & species=="RRGMS" & ctf==5000)
eg <- subset(all.diffs,assetid==8 & species=="WCRR" & ctf==2000)
eg
## Show preference curves for min and max
par(mfcol=c(4,2))
plot(eg)
## Show weights
what.weights(eg)

################################################################################
## Unique preference curves for gwlevel across all assets
plot.unique.prefs(subset(all.diffs,species=="RRGMS"),"gwlevel")

devAskNewPage(T)
plot.unique.prefs(subset(all.diffs,species=="RRGMS"),"duration",ncol=NA)

## all.pref <- subset(all.diffs,species=="RRGMS")
## del <- duplicated(lapply(all.pref,function(x) c(x$pars.max.gwlevel,x$pars.min.gwlevel)))
## all.pref <- all.pref[!del]
## NROW(all.pref)
## par(mfrow=c(4,2))
## plot(all.pref,attribs="gwlevel")



################################################################################

## Redo without gwlevel for WCRR
## TODO: caching of attributes to speed up recalc of prefs
index.all$WCRR_gwlevel.csv <- NULL
all.diffs <- redo_diffs(all.diffs,ecospecies="WCRR",
                        scen="Post90",baseline="Pre90",
                        use.durs=c(T,F),
                        attribs.usesduration=attribs.usesduration,
                        assets=1:nrow(asset.table),getSeqCtfs=getSeqCtfs10
                        )
eg <- subset(all.diffs,assetid==8 & species=="WCRR" & ctf==2000)
what.weights(eg)
