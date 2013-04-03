lp.perf <-
function(xs,ev,bounds=NULL,dir="min",constr=list(),dur=NULL){
  library(lpSolveAPI)
  stopifnot(is.null(dur) | identical(sapply(ev,length),sapply(dur,length)))
  stopifnot(all(ev[[1]]<=max(xs)))
  stopifnot(all(ev[[2]]<=max(xs)))

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
          ## Indicate whether attributes are in this linear
          if(i==nvar-1) { in.ev <- a>=xs[i] & a<=xs[i+1]
                      } else { in.ev <- a>=xs[i] & a<xs[i+1] }
          ## Calculate A and n for this scenarios for this linear
          if(!is.null(dur)){ ##Multiply by duration if required
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

  ## Solve LP
  ##lprec <- make.lp(NROW(constr),length(obj))
  lprec <- make.lp(0,length(obj))
  lp.control(lprec,sense=dir)
  set.objfn(lprec,obj)
  ## Broad bounds
  ## set.bounds(lprec,
  ##            lower=rep(0,length(obj)),
  ##            upper=rep(1,length(obj)))
  ## ## Relationship between breakpoint y values
  ## add.constraint(lprec,c(1,-1,0),"<",0)
  ## add.constraint(lprec,c(0,-1,1),"<",0)
  if(NROW(constr)>0){
    for(i in 1:NROW(constr)){
      cc <- rep(0,length(obj))
      cc[constr[i,1]] <- 1
      cc[constr[i,2]] <- -1
      add.constraint(lprec,cc,constr[i,3],0)
    }
  }
  ##Set limits on y values
  ##set.bounds(lprec,upper=0.1,column=1)
  if(!is.null(bounds)) set.bounds(lprec,upper=bounds$upper,lower=bounds$lower)
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
