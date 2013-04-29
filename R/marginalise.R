marginalise <- function(tabm,form,verbose=F,na.rm=F){
  if (verbose) {
    mar.vars <- setdiff(names(tabm),c(all.vars(form),"value"))
    cat("Retaining variables: ",paste(setdiff(all.vars(form),c("value",".")),collapse=","),"\n")
    cat("Marginalising over variables: ",paste(mar.vars,collapse=","),"\n")
    if(na.rm) cat("Ignoring NAs\n")
  }
  answers <- cast(tabm,
                  formula=form,
                  fun.aggregate=function(s) {
                    s <- unique(as.character(s))
                    if(na.rm) s <- na.omit(s)
                    if(length(s)==1) {return(s)
                    } else if(length(s)==0) { return(NA)
                    } else return("Uncertain")
                  }
                  )
  answers
}
