lp.weights <-
function(p,dir,bounds=NULL,constr=NULL){
  lprec <- make.lp(0,length(p))
  lp.control(lprec,sense=dir)
  set.objfn(lprec,p)
  ## Weights sum to 1
  add.constraint(lprec,rep(1,length(p)),"=",1)
  ## Default bounds
  set.bounds(lprec,upper=rep(1,length(p)),lower=rep(0,length(p)))
  ## Relationship between weights
  if(!is.null(constr)){
    ## If not specified, RHS of constraint is 0 (mainly for backward compatibility)
    if(ncol(constr)==3) constr <- cbind(constr,0)
    for(i in 1:NROW(constr)){
      cc <- rep(0,length(p))
      cc[constr[i,1]] <- 1
      cc[constr[i,2]] <- -1
      add.constraint(lprec,cc,constr[i,3],constr[i,4])
    }
  }
  ## Bounds on weights
  ## TODO: implement as bounds to get duals
  if(!is.null(bounds)) set.bounds(lprec,upper=bounds$upper,
                                  lower=bounds$lower)
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
