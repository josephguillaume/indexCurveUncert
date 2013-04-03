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
  constr <- ifelse(constr>=0,">","<")
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

getPrefConstraints <- getPrefConstraintsMultIndex

getWeightConstraints <- function(attribs){
  list(constr=NULL,bounds=NULL)
}

getPrefConstraintsLists <- function(species,attrib){
  cpt.x <- index.all[[sprintf("%s_%s.csv", species,attrib)]][,1]
  nx <- length(cpt.x)
  constr <- NULL
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
  ## from min.x,max.x,dir
  ## a-b>=0 -> a>b (dir=1 = a>b, dir=-1 = a<b)
  this.monoton <- pref.monoton[[sprintf("%s_%s.csv", species,attrib)]]
  if(!is.null(this.monoton)){
    for(i in 1:nrow(this.monoton)){
      w.x <- which(cpt.x>=this.monoton$min.x[i] & cpt.x<=this.monoton$max.x[i])
      cc <- cbind(expand.grid(a=w.x,b=w.x),status=ifelse(this.monoton$dir[i]==1,">","<"))
      cc <- cc[cc[,1]>cc[,2],]
      constr <- rbind(constr,cc)
    }
  }
  ## Add comparison constraints
  ## from min.x1,max.x1,min.x2,max.x2,dir
  this.comp <- pref.comp[[sprintf("%s_%s.csv", species,attrib)]]
  if(!is.null(this.comp)){
    for(i in 1:nrow(this.comp)){
      w.x1 <- which(cpt.x>=this.comp$min.x1[i] & cpt.x<=this.comp$max.x1[i])
      w.x2 <- which(cpt.x>=this.comp$min.x2[i] & cpt.x<=this.comp$max.x2[i])
      cc <- cbind(expand.grid(a=w.x1,b=w.x2),status=this.comp$dir[i])
      constr <- rbind(constr,cc)
    }
  }
  ##
  return(list(constr=constr,
              bounds=bounds))
}
