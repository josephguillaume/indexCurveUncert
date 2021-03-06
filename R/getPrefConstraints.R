getPrefConstraintsMultIndex <-
function(species,attrib){
  cpt <- index.all[[sprintf("%s_%s.csv", species,attrib)]]
  ##Define absolute constraints
  for(n in names(cpt)[-1]) cpt[,n] <-
    approxes.all[[sprintf("%s_%s.csv", species,attrib)]][[n]](cpt[,1])
  bounds <- list(lower=apply(cpt[,-1,drop=F],1,min),
                 upper=apply(cpt[,-1,drop=F],1,max))

  ## Define relative constraints, a-b>=0
  ss <- expand.grid(1:nrow(cpt),1:nrow(cpt))
  ss <- ss[ss[,1]!=ss[,2],]
  constr <- do.call(rbind,apply(ss,1,function(x) cpt[x[1],-1,drop=F]-cpt[x[2],-1,drop=F]))
  constr <- ifelse(constr>=0,">=","<=")
  constr <- data.frame( ##a=cpt[ss[,1],1],
                       ##b=cpt[ss[,2],1],
                       a=ss[,1],
                       b=ss[,2],
                       status=  apply(constr,1,function(s) {
                         s <- unique(s)
                         if(length(s)==1) return(s)
                         else return(NA)
                       }),stringsAsFactors=F
                       )
  constr <- constr[!is.na(constr$status),]
  list(constr=constr,bounds=bounds)
}

getWeightConstraintsNull <- function(species,attribs){
  list(constr=NULL,bounds=NULL)
}


getWeightConstraintsLists <- function(species,attribs){
  constr <- NULL
  bounds <- list(lower=rep(0,length(attribs)),
                 upper=rep(1,length(attribs)))
  if(exists("weight.comp") && !is.null(weight.comp[[species]])) {
    constr <- weight.comp[[species]]
    ## Remove constraints that refer to attributes not used
    constr <- constr[constr[,1] %in% attribs & constr[,2] %in% attribs,]
    stopifnot(all(constr[,3] %in% c("<=",">=")))
    constr[,1] <- match(constr[,1],attribs)
    constr[,2] <- match(constr[,2],attribs)
    constr[,4] <- constr[,4]*ifelse(constr[,3]=="<=",-1,1)
  }
  if(exists("weight.bounds") && !is.null(weight.bounds[[species]])){
    bbb <- weight.bounds[[species]]
    ## Remove constraints that refer to attributes not used
    bbb <- bbb[bbb[,1] %in% attribs,]
    if(NROW(bbb)>0){
      for(i in 1:nrow(bbb)){
        w.a <- which(attribs==as.character(bbb[i,1]))
        ## Keep tighter bounds
        bounds$lower[w.a] <- max(bounds$lower[w.a],bbb[i,2],na.rm=T)
        bounds$upper[w.a] <- min(bounds$upper[w.a],bbb[i,3],na.rm=T)
      }
    }
  }
  if(NROW(constr)==0) constr <- NULL
  list(constr=constr,
       bounds=bounds)
}

showPrefConstraintsLists <- function(species,attrib){
  cat(sprintf("Constraints on preference curve obtained from lists
for species %s, attrib %s\n",species,attrib))

  cat("pref.bounds\n")
  if(!exists("pref.bounds")){
    cat("No bounds\n")
    pref.bounds <<- NULL
  } else {
    this.bounds <- pref.bounds[[sprintf("%s_%s.csv", species,attrib)]]
    if(is.null(this.bounds)){
      cat("No bounds\n")
    } else {
      print(this.bounds)
      for(i in 1:nrow(this.bounds)){
        cat(sprintf("Suitability must be between %f and %f for attribute values between %f and %f\n",
                    this.bounds$min.y[i],this.bounds$max.y[i],
                    this.bounds$min.x[i],this.bounds$max.x[i]
                    ))
      }
    }
  } ## pref.bounds

  cat("pref.monoton\n")
  if(!exists("pref.monoton")){
    cat("No constraints on monotonicity - suitability can go up or down unrestricted\n")
    pref.monoton <<- NULL
  } else {
    this.monoton <- pref.monoton[[sprintf("%s_%s.csv", species,attrib)]]
    if(is.null(this.monoton)){
      cat("No constraints on monotonicity - suitability can go up or down unrestricted\n")
    } else {
      ## Default to non-strict monotonicity
      if(is.null(this.monoton$min.step)) this.monoton$min.step <- 0
      print(this.monoton)
      for(i in 1:nrow(this.monoton)){
        cat(sprintf("Suitability must go %s by at least %f for attribute values between %f and %f\n",
                    ifelse(this.monoton$dir[i]==1,"up","down"),this.monoton$min.step[i],
                    this.monoton$min.x[i],this.monoton$max.x[i]
                    ))
      }
    }
  } ##pref.monoton

  cat("pref.comp\n")
  if(!exists("pref.comp")){
    cat("No comparison constraints - suitability of different attribute values are not directly compared\n")
    pref.comp <<- NULL
  } else {
    this.comp <- pref.comp[[sprintf("%s_%s.csv", species,attrib)]]
    if(is.null(this.comp)){
      cat("No comparison constraints - suitability of different attribute values are not directly compared\n")
    } else {
      ## Default to weak comparison
      if(is.null(this.comp$min.gap)) this.comp$min.gap <- 0
      print(this.comp)
      for(i in 1:nrow(this.comp)){
        cat(sprintf("Suitability of attribute values between %f and %f must be %s than suitability of attribute values between %f and %f, by at least %f\n",
                    this.comp$min.x1[i],this.comp$max.x1[i],
                    ifelse(this.comp$dir[i]=="<","less","greater"),
                    this.comp$min.x2[i],this.comp$max.x2[i],
                    this.comp$min.gap[i]
                    ))
      }
    }
  } ## pref.comp

  cat("pref.smooth\n")
  if(!exists("pref.smooth")){
    cat("No smoothness constraints - suitability can vary abruptly\n")
    pref.smooth <<- NULL
  } else {
    this.smooth <- pref.smooth[[sprintf("%s_%s.csv", species,attrib)]]
    if(is.null(this.smooth)){
      cat("No smoothness constraints - suitability can vary abruptly\n")
    } else {
      print(this.smooth)
      for(i in 1:nrow(this.smooth)){
        cat(sprintf("Suitability can only vary by between %f and %f per unit change in attribute value, for attribute values between %f and %f\n",
                    this.smooth$min.step[i],this.smooth$max.step[i],
                    this.smooth$min.x[i],this.smooth$max.x[i]
                    ))
      }
    }
  } ##pref.smooth

  cat("\n")
  ## if(!exists("pref.single.extreme.notp")){
  ##   warning("pref.single.extreme.notp not found, setting to NULL")
  ##   pref.single.extreme.notp <<- NULL
  ## }
  invisible("See output")

}

getPrefConstraintsLists <- function(species,attrib){
  cpt.x <- index.all[[sprintf("%s_%s.csv", species,attrib)]][,1]
  nx <- length(cpt.x)
  constr <- NULL
  if(!exists("pref.bounds")){
    warning("pref.bounds not found, no bounds set")
    pref.bounds <<- NULL
  }
  if(!exists("pref.monoton")){
    warning("pref.monoton not found, no monotonicity constraints set")
    pref.monoton <<- NULL
  }
  if(!exists("pref.comp")){
    warning("pref.comp not found, no comparison constraints set")
    pref.comp <<- NULL
  }
  if(!exists("pref.smooth")){
    warning("pref.smooth not found, no smoothness constraints set")
    pref.smooth <<- NULL
  }
  ## if(!exists("pref.single.extreme.notp")){
  ##   warning("pref.single.extreme.notp not found, setting to NULL")
  ##   pref.single.extreme.notp <<- NULL
  ## }

  ## Convert real-valued bounds
  bounds <- list(lower=rep(0,nx),upper=rep(1,nx))
  this.bounds <- pref.bounds[[sprintf("%s_%s.csv", species,attrib)]]
  if(!is.null(this.bounds)){
    for(i in 1:nrow(this.bounds)){
      w.x <- which(cpt.x>=this.bounds$min.x[i] & cpt.x<=this.bounds$max.x[i])
      ## Keep the tighter of the bounds?
      ## TODO: alternative?
      bounds$lower[w.x] <- pmax(bounds$lower[w.x],this.bounds$min.y[i],na.rm=T)
      bounds$upper[w.x] <- pmin(bounds$upper[w.x],this.bounds$max.y[i],na.rm=T)
    }
  }
  ## Add monotonicity constraints
  ## from min.x,max.x,dir,min.step
  ## a-b>=min.step -> a>b+min.step (dir=1 = a>b, dir=-1 = a<b)
  this.monoton <- pref.monoton[[sprintf("%s_%s.csv", species,attrib)]]
  if(!is.null(this.monoton)){
    ## Default to non-strict monotonicity
    if(is.null(this.monoton$min.step)) this.monoton$min.step <- 0
    for(i in 1:nrow(this.monoton)){
      w.x <- which(cpt.x>=this.monoton$min.x[i] & cpt.x<=this.monoton$max.x[i])
      cc <- cbind(expand.grid(a=w.x,b=w.x),
                  status=ifelse(this.monoton$dir[i]==1,">=","<="),
                  ## TODO: min.step should be slope - need to be independent of breakpoint x values
                  min.gap=this.monoton$dir[i]*this.monoton$min.step[i]
                  )
      cc <- cc[cc[,1]>cc[,2],]
      cc$min.gap <- cc$min.gap*(cpt.x[cc$a]-cpt.x[cc$b])
      constr <- rbind(constr,cc)
    }
  }
  ## Add smoothness constraints
  ## min.step*(xb-xa)<=a-b<=max.step*(xb-xa)
  this.smooth <- pref.smooth[[sprintf("%s_%s.csv", species,attrib)]]
  if(!is.null(this.smooth)){
    for(i in 1:nrow(this.smooth)){
      w.x <- which(cpt.x>=this.smooth$min.x[i] & cpt.x<=this.smooth$max.x[i])
      ## Max slope ya-yb<=min.step, xa>xb
      cc1 <- cbind(expand.grid(a=w.x,b=w.x),status="<=")
      cc1$min.gap=this.smooth$max.step[i]*(cpt.x[cc1$a]-cpt.x[cc1$b])
      cc1 <- cc1[cc1[,1]>cc1[,2],]
      ## Min slope ya-yb>=min.step, xa>xb
      cc2 <- cbind(expand.grid(a=w.x,b=w.x),status=">=")
      cc2$min.gap=this.smooth$min.step[i]*(cpt.x[cc2$a]-cpt.x[cc2$b])
      cc2 <- cc2[cc2[,1]>cc2[,2],]
      constr <- rbind(constr,cc1,cc2)
    }
  }

  ## Add comparison constraints
  ## from min.x1,max.x1,min.x2,max.x2,dir,min.gap
  this.comp <- pref.comp[[sprintf("%s_%s.csv", species,attrib)]]
  if(!is.null(this.comp)){
    ## Default to weak comparison
    if(is.null(this.comp$min.gap)) this.comp$min.gap <- 0
    for(i in 1:nrow(this.comp)){
      w.x1 <- which(cpt.x>=this.comp$min.x1[i] & cpt.x<=this.comp$max.x1[i])
      w.x2 <- which(cpt.x>=this.comp$min.x2[i] & cpt.x<=this.comp$max.x2[i])
      cc <- cbind(expand.grid(a=w.x1,b=w.x2),
                  status=this.comp$dir[i],
                  min.gap=-this.comp$min.gap[i]
                  )
      constr <- rbind(constr,cc)
    }
  }
  ##
  ## Add single extreme constraint

  return(list(constr=constr,
              bounds=bounds))
}

cachePreferences <- function(index.names,replace=F){
  if(!exists("cached.pref")) cached.pref <<- list()
  new.cached.pref <- lapply(index.names,
                            function(nn){
                              nn <- gsub(".csv","",nn,fixed=T)
                              nn <- strsplit(nn,"_")[[1]]
                              getPrefConstraints(nn[1],nn[2])
                            })
  names(new.cached.pref) <- index.names
  if(!replace & length(intersect(names(cached.pref),names(new.cached.pref))!=0))
    stop("Already have cached constraints for some indexes and replace=F, merging not currently supported")
  ## TODO: support merging
  cached.pref <<- modifyList(cached.pref,new.cached.pref)
}

## Caching to avoid recomputation from lists
## Combine outputs from cache and backup

getPrefConstraintsCached <- function(species,attrib,backup=NA){
  constr <- cached.pref[[sprintf("%s_%s.csv", species,attrib)]]
  if(is.null(constr) & is.function(backup)) constr <- backup(species,attrib)
  constr
}
## If not found, use no constraints
##getPrefConstraints <- getPrefConstraintsCached
## If not found, raise error
## getPrefConstraints <- function(...)
##   getPrefConstraintsCached(...,
##                            backup=function(...) stop("Not found"))
## If not found, use lists
## getPrefConstraints <- function(...)
##   getPrefConstraintsCached(...,backup=getPrefConstraintsLists)

## Return combination of cached and new extra
getPrefConstraintsMergeWithCache <- function(species,attrib,extra){
  constr <- cached.pref[[sprintf("%s_%s.csv", species,attrib)]]
  constr2 <- extra(species,attrib)
  ## If none, just return extra
  if(is.null(constr)) return(constr2)
  ## Add min.gap column if missing
  if(ncol(constr$constr)==3)
    constr$constr <- cbind(constr$constr,min.gap=0)
  if(!is.null(constr2$constr) && ncol(constr2$constr)==3)
    constr2$constr <- cbind(constr2$constr,min.gap=0)
  ## Keep the tighter of the bounds
  constr$bounds$lower <- pmax(constr$bounds$lower,constr2$bounds$lower,na.rm=T)
  constr$bounds$upper <- pmin(constr$bounds$upper,constr2$bounds$upper,na.rm=T)
  ## Keep all constr
  constr$constr <- rbind(constr$constr,constr2$constr)
  constr
}
## getPrefConstraints <- function(...)
##   getPrefConstraintsMergeWithCache(...,extra=getPrefConstraintsLists)
