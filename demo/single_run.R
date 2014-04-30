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

checkAttributeRanges("Pre90",specieslist,attribs=names(attribs.usesduration))
checkAttributeRanges("Post90",specieslist,attribs=names(attribs.usesduration))



################################################################################


st <- proc.time()
xx <- envindex.diff.qp(scen="Pre90",baseline="Post90",
                       ecospecies=specieslist,
                       assetid=1,ctf=asset.table[1,7],
                       use.durs=c(T,F),
                       attribs.usesduration = attribs.usesduration
                       )
proc.time()-st

## Printing shows data.frame of results and settings
print(xx)
## Results can be subsetted, and return an envindex.bound object
subset(xx,subset=species=="BBMS" & use.dur==TRUE)

## Plot method shows preference curve for given attrib and subset
par(mai=c(0.55,0.5,0.3,0.3),cex=0.55)
plot(xx,attribs="duration",subset=species=="BBMS" & use.dur==FALSE,main=NA)
legend("topright",
       c("Constraints","Favouring baseline","Favouring scenario"),
       fill=c(grey(0.9),NA,NA),
       col=c(NA,red(),green()),
       lty=c(NA,"dashed","dotted"),
       border=NA,lwd=c(NA,3,3))

## Multiple plots can be made at once, for different species
subset(xx,subset=use.dur==TRUE)

par(mfrow=c(4,2))
plot(xx,attribs="duration",subset=use.dur==TRUE)

what.weights(xx)

## What weights would make the result certain?
plot.weight.classes(xx[[1]],"max",attribs.usesduration)
