library(indexCurveUncert)
##library(hydromad) ##eventseq
loadIndexes("X:/phd/papers/namoi ua/Namoi model/Inputs EMS paper/index/")
loadHydro("X:/phd/papers/namoi ua/Namoi model/Inputs EMS paper/",c("Pre90","Post90"))
loadAssets("X:/phd/papers/namoi ua/Namoi model/Inputs EMS paper/ctf_v4.csv")

specieslist <- c("RRGMS", "RRGRR", "BBMS", "BBRR", "LGMS", "LGRR", "WCMS", "WCRR")
attribs.usesduration = c(timing = "duration", duration = "duration", 
                         dry = "duration",gwlevel=NA)

################################################################################

## Extend  indices TODO: generalise
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
## Vary all

all.diffs <- vary_all(scen="Post90",baseline="Pre90",ecospecies=specieslist,
                      use.durs=c(T,F),
                      attribs.usesduration=attribs.usesduration,
                      assets=1:nrow(asset.table),getSeqCtfs=getSeqCtfs10
                      )

##save(all.diffs,file="vary_all_qp.Rdata")


################################################################################
## Visualisation
##load("vary_all_qp.Rdata")

library(ggplot2)
library(reshape)

## Extract data.frame
###tab2 <- as.data.frame(subset(all.diffs,subset=!is.na(diff.min))) ##TODO: prefer this kind of syntax?
tab <- do.call(rbind,lapply(subset(all.diffs,subset=!is.na(diff.min)),
                            function(r) as.data.frame(r[c("assetid","ctf","species","diff.min","diff.max","use.dur")])))
## Convert to long form
tabm <- melt(tab,id.var=c("assetid","ctf","species","use.dur"))
## Convert continuous variable to discrete - which scenario is favoured
##tabm$value <- ifelse(tabm$value>=0,"Scenario","Baseline")
tabm$value <- ifelse(tabm$value>0,"Scenario","Baseline or =")

## Check for uncertainty
answers <- marginalise(tabm,assetid+species+ctf~.,verbose=T)

## Plot from viz_vary_all.R

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
## lookup <- c("RRGMS"="River red gum\nMaintenance\nand survival",
##             "RRGRR"="River red gum\nRegeneration\nand reproduction",
##             "BBMS"="Black box\nMaintenance\nand survival",
##             "BBRR"="Black box\nRegeneration\nand reproduction",
##             "LGMS"="Lignum\nMaintenance\nand survival",
##             "LGRR"="Lignum\nRegeneration\nand reproduction",
##             "WCMS"="Water couch\nMaintenance\nand survival",
##             "WCRR"="Water couch\nRegeneration\nand reproduction"
##             )
answers$species <- lookup[as.character(answers$species)]

## Convert CTF levels from asset.table for comparison
all.ctf <- melt(asset.table[,3:6])
names(all.ctf) <- c("assetid","variable","value")

##answers <- subset(answers,assetid %in% c("Barbers lagoon","Gunnedah"))


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

## TODO: select point
subset(all.diffs,assetid==1 & species=="RRGMS" & ctf==5000)
eg <- subset(all.diffs,assetid==1 & species=="RRGMS" & ctf==5000)[1]
eg
## Show preference curves for min and max
par(mfrow=c(3,1))
plot(eg)
what.weights(eg)

################################################################################

## TODO: plot weights
## TODO: compare final result with snow implementation (is correct for given pref and weights)
