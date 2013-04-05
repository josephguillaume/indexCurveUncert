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

getWeightConstraintsNull <- function(attribs){
  list(constr=NULL,bounds=NULL)
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
      constr <- rbind(constr,cc)
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
