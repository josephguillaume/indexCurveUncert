## ev and dur may both have list elements of length zero which may or may not be NULL.
lp.perf <-
function(xs,ev,bounds=NULL,dir="min",constr=list(),dur=NULL){
  library(lpSolveAPI)
  if(!(length(dur)==0 | identical(sapply(ev,length),sapply(dur,length))))
     stop("Lengths of ev and dur do not match for some scenario")
  if((length(ev[[1]])>0 && !all(ev[[1]]<=max(xs)))|(length(ev[[2]])>0 && !all(ev[[2]]<=max(xs))))
    stop(sprintf("Maximum attribute value of events (%.2f) is not less than or equal to maximum x value of index curve (%.2f)",
                 max(c(ev[[1]],ev[[2]])),max(xs)))

  nvar <- length(xs)
  ## Add x indices of breakpoints to vals
  vals <- xs
  names(vals) <- sprintf("x%d",1:nvar-1)
  ##Calculate total diff. in attributes, and difference in num events
  for(i in 1:(nvar-1)){ ## For each piece-wise linear
    ## Calculate A and n for all scenarios for this linear,
    ##  each scenario in a column in order
      x <- sapply(1:length(ev),function(aidx) { ##For each scenario
          a <- ev[[aidx]] ##Attributes of each event
          ## If no events, return 0
          if(length(a)==0){
            o <- c(0,0)
            names(o) <- c(sprintf("A%d",i),sprintf("n%d",i))
            o
          }
          ## Indicate whether attributes are in this linear
          if(i==nvar-1) { in.ev <- a>=xs[i] & a<=xs[i+1]
                      } else { in.ev <- a>=xs[i] & a<xs[i+1] }
          ## Calculate A and n for this scenario for this linear
          if(length(dur)>0){ ##Multiply by duration if required
              o <- c(A=sum(a[in.ev]*dur[[aidx]][in.ev]),
                     n=sum(dur[[aidx]][in.ev]))
          } else { o <- c(sum(a[in.ev]),n=length(which(in.ev))) }
          names(o) <- c(sprintf("A%d",i),sprintf("n%d",i))
          o
      })
      ##print(apply(x,1,diff))
      ## Add dA,dN to vals, as 2nd scenario-1st scenario in ev
      vals <- c(vals,apply(x,1,diff))
  }
  ## Calculate objective function
  obj <- sapply(0:(nvar-1),function(i){
    ##if not first, i.e. not 0
    ##Ai/(xi-xi1)-(xi*ni)/(xi-xi1)+ni
    if(i==0) { cc1 <- NULL
             } else{
               cc1 <- gsub("i",i,"Ai/(xi-xj)-(xi*ni)/(xi-xj)+ni")
               cc1 <- gsub("j",i-1,cc1)
             }
    ##if not last, using i+1
    ##-Ai/(xi-xi1)+(xi*ni)/(xi-xi1)
    if(i==nvar-1) { cc2 <- NULL
                } else {
                  cc2 <- gsub("i",i,"-Ak/(xk-xi)+(xk*nk)/(xk-xi)")
                  cc2 <- gsub("k",i+1,cc2)
                }
    cc <- paste(cc1,cc2,sep="")
    with(as.list(vals),eval(parse(text=cc)))
  })
  stopifnot(all(is.finite(obj)))
  ## Solve LP
  lprec <- make.lp(NROW(constr),nvar)
  ## Broad bounds
  ## set.bounds(lprec,
  ##            lower=rep(0,length(obj)),
  ##            upper=rep(1,length(obj)))
  ## ## Relationship between breakpoint y values
  ## add.constraint(lprec,c(1,-1,0),"<",0)
  ## add.constraint(lprec,c(0,-1,1),"<",0)
  if(NROW(constr)>0){
    ##1a-1b<>rhs = a<>b+rhs
    ## If not specified, RHS of constraint is 0 (mainly for backward compatibility)
    if(ncol(constr)==3) constr <- cbind(constr,0)
    ## nvar x nconstr matrix
    cc <- t(apply(constr[,1:2],1,
                  function(i) (1:nvar==i[1])-(1:nvar==i[2])))
    for(i in 1:ncol(cc)){
        set.column(lprec,i,cc[,i])
        set.constr.type(lprec,as.character(constr[ ,3]))
        set.constr.value(lprec,constr[,4])
    }
  }
  ##Set limits on y values
  ##set.bounds(lprec,upper=0.1,column=1)
  if(!is.null(bounds)) set.bounds(lprec,upper=bounds$upper,lower=bounds$lower)
  lp.control(lprec,sense=dir)
  set.objfn(lprec,obj)
  st=solve(lprec)
  if(st!=0) stop(sprintf("lpSolve 'solve' status code was %d",st))

  ##browser()
  ##ss <- get.sensitivity.rhs(lprec)
  ##cbind(dual=ss$duals[which(ss$duals!=0)],from=ss$dualsfrom[which(ss$duals!=0)],till=ss$dualstill[which(ss$duals!=0)]) ##unit change in variable on obj
  ## get.sensitivity.objex(lprec) ##dep on coefs. difficult to interpret, given mix of dA,dN etc
  ##get.constraints(lprec)

  list(
       obj=get.objective(lprec),
       ys=get.variables(lprec),
       duals=get.sensitivity.rhs(lprec)
       )
}
