library(indexCurveUncert)
library(hydromad) ##eventseq
loadIndexes("X:/phd/papers/namoi ua/Namoi model/Inputs EMS paper/index/")
loadHydro("X:/phd/papers/namoi ua/Namoi model/Inputs EMS paper/",c("Pre90","Post90"))
loadAssets("X:/phd/papers/namoi ua/Namoi model/Inputs EMS paper/ctf_v4.csv")

specieslist <- c("RRGMS", "RRGRR", "BBMS", "BBRR", "LGMS", "LGRR", "WCMS", "WCRR")

st <- proc.time()
xx <- envindex.diff.qp(scen="Pre90",baseline="Post90",
                       ecospecies=specieslist,
                       assetid=1,ctf=asset.table[1,7],
                       use.durs=c(T,F))

print(xx)
subset(xx,subset=species=="BBMS" & use.dur==TRUE)

##win.metafile("bbms_duration_polygonconstraint_legend.emf",width=3.54,height=2.3)
par(mai=c(0.55,0.5,0.3,0.3),cex=0.55)
## TODO: wrong sign
plot(xx,attribs="duration",subset=species=="BBMS" & use.dur==FALSE,main=NA)
legend("topright",c("Constraints","Favouring SYA","Favouring SYP"),fill=c(grey(0.9),NA,NA),col=c(NA,red,green),lty=c(NA,"dashed","dotted"),border=NA,lwd=c(NA,3,3))
##dev.off()

subset(xx,subset=use.dur==TRUE)

par(mfrow=c(3,2))
plot(xx,attribs="duration",subset=use.dur==TRUE)

