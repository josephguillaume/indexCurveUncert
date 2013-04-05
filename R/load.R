## index.all
## List
## $ {species}_{attrib} data.frame
## attribute values (x coordinate)
## separate index curves
## ...
## approxes.all
loadIndexes <- function(fname){
  new.index.all <- lapply(dir(fname,
                              pattern=".csv",full.names=T), read.csv, header=T)
  names(new.index.all) <- dir(fname,pattern=".csv")
  ## Merge indices
  if(exists("index.all")){
    print("Merging indices")
    duplicate.names <- intersect(names(new.index.all),names(index.all))
    if(length(duplicate.names)>0){
      for(n in duplicate.names){
        dup.cols <- intersect(names(index.all[[n]])[-1],names(new.index.all[[n]])[-1])
        if(length(dup.cols)>0)
          stop(sprintf("Columns of new %s index have same names as old. Not supported",n))
        ## Merge new columns to old columns. If different number of bkpts, fills with NAs
        index.all[[n]] <<- merge(index.all[[n]],new.index.all[[n]],all=T,by=1)
        ## Remove from new - already added to old
        new.index.all[[n]] <- NULL
      }
    }
    ## Merge remaining - shouldn't be any duplicates
    index.all <- modifyList(index.all,new.index.all)
  } else { index.all <- new.index.all }
  ## Used in plot and getPrefConstraints
  approxes.all <- lapply(index.all,
                         function(cpt) {
                           a <- lapply(2:ncol(cpt),function(i) approxfun(cpt[,1], cpt[,i], rule=2))
                           names(a) <- names(cpt)[-1]
                           a
                         })
  index.all <<- index.all
  approxes.all <<- approxes.all
}

loadHydro <- function(fname,scenariolist){
  library(zoo)
  if(exists("all.hydroinputlist")){
    warning("Already loaded. rm(all.hydroinputlist) to allow reloading")
  } else {
    cat("Loading hydrological data\n")
    all.hydroinputlist <- NULL
    for (s in scenariolist){
      hydroinputlist <- lapply(dir(paste(fname, s, sep="/"), pattern=".csv", full.names=T),
                               ##read.zoo, header=T, sep=",")
                               read.zoo, header=T, sep=" ")
      ##names(hydroinputlist) <- gsub(".csv","",dir(paste(fname, s, sep="/"), pattern=".csv"),fixed=T)
      names(hydroinputlist) <- dir(paste(fname, s, sep="/"), pattern=".csv")
      all.hydroinputlist[[s]] <- hydroinputlist
    }
  }
  all.hydroinputlist <<- all.hydroinputlist
}

loadAssets <- function(fname){
  asset.table <<- read.csv(fname, header=T)
}

green<- function() "#009246"
red<- function() "#CE2B37"

capwords<-
function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s,1,1)),
                         {s <- substring(s,2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
