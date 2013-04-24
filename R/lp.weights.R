lp.weights <-
function(p,dir,bounds=NULL,constr=NULL){
  lprec <- make.lp(NROW(constr)+1,length(p))
  ## Relationship between weights
  cc <- NULL
  if(!is.null(constr)){
    ## If not specified, RHS of constraint is 0 (mainly for backward compatibility)
    if(ncol(constr)==3) constr <- cbind(constr,0)
    ## nvar x nconstr matrix
    cc <- t(apply(constr[,1:2],1,
                  function(i) (1:length(p)==i[1])-(1:length(p)==i[2])))
  }
  ## Weights sum to 1
  cc <- rbind(cc,rep(1,length(p)))
  ## Set constraints, incl weights sum to 1
  for(i in 1:ncol(cc)){
      set.column(lprec,i,cc[,i])
      set.constr.type(lprec,c(as.character(constr[ ,3]),"="))
      set.constr.value(lprec,c(constr[,4],1))
  }
  ## Default bounds
  set.bounds(lprec,upper=rep(1,length(p)),lower=rep(0,length(p)))
  ## Bounds on weights
  ## TODO: implement as bounds to get duals
  if(!is.null(bounds)) set.bounds(lprec,upper=bounds$upper,
                                  lower=bounds$lower)
  lp.control(lprec,sense=dir)
  set.objfn(lprec,p)
  st=solve(lprec)
  if(st!=0) stop(sprintf("lpSolve 'solve' status code was %d",st))
  ws <- get.variables(lprec)
  names(ws) <- names(p)
  list(
       obj=get.objective(lprec),
       ws=ws,
       duals=get.sensitivity.rhs(lprec)
       )
}
