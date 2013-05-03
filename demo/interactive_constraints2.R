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

## 3. Set pref constraints method
## i. Cache mult-index preferences
## TODO: don't clobber existing index.all and getPrefConstraints
getPrefConstraints <- getPrefConstraintsMultIndex
cachePreferences(grep("test",names(index.all),invert=T,value=T)) ##not for test

## ## ii. Cache list preferences
## ## assume indexes has correct breakpoints
## getPrefConstraints <- getPrefConstraintsLists
## cachePreferences(grep("^test",names(index.all),value=T)) ##for test

## Get constraints from cache.
## Use cache and add extra constraints from lists
getPrefConstraints <- function(...)
  getPrefConstraintsMergeWithCache(...,extra=getPrefConstraintsLists)


## 2. specify scenarios to run and results to print
attribs.usesduration = c(timing = "duration", duration = "duration", 
                         dry = "duration",gwlevel=NA)
go <- function(){
  ## Note dur and nodur are NOT the same
  xx <- envindex.diff.qp(scen="Pre90",baseline="Post90",
                         ecospecies="RRGMS",
                         assetid=1,ctf=5000,
                         use.durs=c(T,F),
                         attribs.usesduration = attribs.usesduration
                         )
  print(xx)
  par(mfcol=c(4,2))
  plot(xx)
  print(what.weights(xx))
  invisible(xx)
}

## 3. go with default constraints
go()


## 4. change constraints and re-run

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

go()

## ii.
##min.x,max.x,min.y,max.y all inclusive
pref.bounds <- list()
## No gw access beyond 12m
pref.bounds$RRGMS_gwlevel.csv <- data.frame(min.x=12,max.x=Inf,min.y=NA,max.y=0)
## GW access good between 2 and 5 m
pref.bounds$RRGMS_gwlevel.csv <- rbind(pref.bounds$RRGMS_gwlevel.csv,
                                      data.frame(min.x=2,max.x=5,min.y=1,max.y=1))
showPrefConstraintsLists("RRGMS","gwlevel")
go()

## iii.
## Monotonicity constraints
##min.x,max.x,dir -> f(min.x)<=f(max.x)
pref.monoton <- list()
pref.monoton$RRGMS_gwlevel.csv <- data.frame(min.x=5,max.x=12,dir=-1,min.step=0)
showPrefConstraintsLists("RRGMS","gwlevel")
go()


### iv. Weight constraints
weight.comp <- list()
weight.comp$RRGMS <- data.frame(a="gwlevel",
                                b=setdiff(names(attribs.usesduration),c("gwlevel","duration")),
                                status="<=",min.gap=0.05)

go()

## v. Constrain duration

pref.smooth <- list()
## TODO: effect depends on number of breakpoints and scale of x axis
pref.smooth$RRGMS_duration.csv <- data.frame(min.x=0,max.x=100,min.step=-0.02,max.step=0.02)

pref.bounds$RRGMS_duration.csv <- data.frame(min.x=100,max.x=100,min.y=1,max.y=1)
showPrefConstraintsLists("RRGMS","duration")

weight.bounds <- list()
weight.bounds$RRGMS <- data.frame(a="duration",min=NA,max=0)

## TODO: could plot what weights would give solution

##diff(x[[1]]$pars.min.duration)/diff(index.all$RRGMS_duration.csv[,1])

go()

################################################################################
## Constraints used
## Real terms
pref.bounds
pref.monoton
pref.comp
weight.comp

getWeightConstraints("RRGMS",names(attribs.usesduration))

specieslist <- c("RRGMS")
mapply(showPrefConstraintsLists,species=specieslist,attrib=names(attribs.usesduration))

################################################################################
## Time effect of caching
## If little difference, main advantage is ability to combine
test <- function(species){
  xx <- envindex.diff.qp(scen="Pre90",baseline="Post90",
                         ##ecospecies="test",
                         ecospecies=species,
                         assetid=1,ctf=5000,
                         use.durs=c(T,F),
                         attribs.usesduration = c(timing="duration",gwlevel=NA)
                         )
}
getPrefConstraints <- getPrefConstraintsCached
system.time(replicate(10,test("test")))
##   8.72    0.20    8.97 
getPrefConstraints <- getPrefConstraintsLists
system.time(replicate(10,test("test")))
##   8.74    0.22    9.04 


getPrefConstraints <- getPrefConstraintsCached
system.time(replicate(10,test("RRGMS")))
##   2.77    0.11    2.99 
getPrefConstraints <- getPrefConstraintsMultIndex
system.time(replicate(10,test("RRGMS")))
##   4.87    0.09    5.33 
