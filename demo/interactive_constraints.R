## Interactively add preference constraints starting from a blank slate

## 1. Load data
library(indexCurveUncert)
loadHydro("X:/phd/papers/namoi ua/Namoi model/Inputs EMS paper/",c("Pre90","Post90"))
loadAssets("X:/phd/papers/namoi ua/Namoi model/Inputs EMS paper/ctf_v4.csv")

## 2. Specify break-point x values
index.all <- list()
index.all$test_dry.csv <- data.frame(X1=c(seq(0,5000,length.out=20),25000))
index.all$test_gwlevel.csv <- data.frame(X1=c(seq(0,30,length.out=100)))
index.all$test_timing.csv <- data.frame(X1=1:12)

## 3. Set pref constraints method
getPrefConstraints <- getPrefConstraintsLists

## 2. specify scenarios to run and results to print
go <- function(){
  ## Note dur and nodur are NOT the same
  xx <- envindex.diff.qp(scen="Pre90",baseline="Post90",
                         ecospecies="test",
                         assetid=1,ctf=5000,
                         use.durs=c(T,F),
                         attribs.usesduration = c(timing="duration",gwlevel=NA)
                         )
  print(xx)
  par(mfrow=c(2,2))
  plot(xx)
  print(what.weights(xx))
}

## 3. go with no constraints
go()

## 4. add constraints and re-run

## i.
##min.x,max.x,min.y,max.y all inclusive
pref.bounds <- list()
## Best timing is september,october
pref.bounds$test_timing.csv <- data.frame(min.x=9,max.x=10,min.y=1,max.y=NA)
## No gw access beyond 12m
pref.bounds$test_gwlevel.csv <- data.frame(min.x=12,max.x=Inf,min.y=NA,max.y=0)
## GW access good between 2 and 5 m
pref.bounds$test_gwlevel.csv <- rbind(pref.bounds$test_gwlevel.csv,
                                      data.frame(min.x=2,max.x=5,min.y=1,max.y=1))
go()

## ii.
## Monotonicity constraints
##min.x,max.x,dir -> f(min.x)<=f(max.x)
## TODO: allow *strictly* incr/decr
pref.monoton <- list()
pref.monoton$test_gwlevel.csv <- data.frame(min.x=5,max.x=12,dir=-1,min.step=0)
pref.monoton$test_timing.csv <- data.frame(min.x=5,max.x=9,dir=1,min.step=1e-3)

go()

## iii.
## Comparison constraints
## min.x1,max.x1,min.x2,max.x2,dir,TODO dist
pref.comp <- list()
pref.comp$test_timing.csv <- data.frame(min.x1=2,max.x1=2,min.x2=4,max.x2=4,dir="<",min.gap=0.1)

go()

################################################################################
## Constraints used
## Real terms
pref.bounds
pref.monoton
pref.comp

## Breakpoint terms
getPrefConstraintsLists("test","timing")



## TODO: multiplot should warn & use devAskNewPage?
## TODO: missing pref curves are silently ignored
## TODO: allow multiple getPrefConstraints 

################################################################################
##Look at previous constraints
loadIndexes("C:\\temp\\My Dropbox\\temp\\index_all")
pdf("old_constraints.pdf")
for(species in specieslist){
  for(attrib in c("dry","duration","timing")){
    cpt <- index.all[[sprintf("%s_%s.csv", species,attrib)]]
    if (is.null(cpt)) next
    constr <- getPrefConstraints(species,attrib)
    plot(cpt[,1],rep(NA,nrow(cpt)),ylim=c(0,1),
         main=sprintf("%s %s", species,attrib)
         )
    polygon(x=c(cpt[,1],rev(cpt[,1])),
            y=c(constr$bounds$upper,
              rev(constr$bounds$lower)),col=grey(0.9),
            border=NA)
    ##print(constr$bounds)
  }
}
dev.off()
