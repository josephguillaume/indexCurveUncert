getPrefConstraints <-
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

getWeightConstraints <- function(attribs){
  list(constr=NULL,bounds=NULL)
}
