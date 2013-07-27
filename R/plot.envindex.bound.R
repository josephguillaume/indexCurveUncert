plot.envindex.bound <-
function(x,y,...,attribs=NA,subset=T,
         main=NULL
         ){
  if(!exists("getPrefConstraints")){
    warning("function getPrefConstraints not set, using getPrefConstraintsMultIndex")
    assign("getPrefConstraints",getPrefConstraintsMultIndex,env=.GlobalEnv)
  }

  ## TODO: should be possible to use subset within another function
  ##x <- subset(x,subset)
  e <- substitute(subset)
  x <- x[sapply(x,function(o) {
    r <- eval(e,o,parent.frame())
    r <- r & !is.na(r)
  })]
  wanted.attribs <- attribs
  wanted.main <- main
  for(o in x){
    if(is.na(wanted.attribs)) attribs <- names(o$pars.min.weights)
    for(attrib in attribs){
      cpt <- index.all[[sprintf("%s_%s.csv", o$species,attrib)]]
      if(is.null(cpt)) next
      constr <- getPrefConstraints(o$species,attrib)
      plot(cpt[,1],
           o[[sprintf("pars.min.%s",attrib)]],
           type="l",col=red(),
           ylab="Preference",ylim=c(0,1),
           xlab=capwords(attrib),
           lty="dashed",lwd=3
           )
      polygon(x=c(cpt[,1],rev(cpt[,1])),
              y=c(constr$bounds$upper,
                rev(constr$bounds$lower)),col=grey(0.9),
              border=NA)
      abline(v=cpt[,1],lty="dashed",col="grey")
      lines(cpt[,1],
            o[[sprintf("pars.min.%s",attrib)]],
            type="l",col=red(),lty="dashed",lwd=3
            )
      lines(cpt[,1],
            o[[sprintf("pars.max.%s",attrib)]],
            type="l",col=green(),lty="dotted",lwd=3
            )
      ## for(a in approxes.all[[sprintf("%s_%s.csv", o$species,tolower(attrib))]])
      ##     plot(a,from=min(cpt[,1]),to=max(cpt[,1]),col="grey",add=T)
      ##segments(x0=cpt[,1],x1=cpt[,1],y0=constr$bounds$lower,y1=constr$bounds$upper)
      ##text(x=cpt[,1],y=constr$bounds$upper,label=1:nrow(cpt),pos=3)
      if(is.null(wanted.main))
        main <-sprintf("%s %s\n%s", o$species,attrib,
                       ifelse(o$use.dur,
                              "As average of event",
                              "As total for event"))
      title(main=main)
    }
  }
}



plot.unique.prefs <- function(diffs,attrib,ncol=4){
  del <- duplicated(lapply(diffs,function(x)
                           do.call(c,x[sprintf("pars.%s.%s",
                                               c("max","min"),
                                               attrib)])))
  diffs <- diffs[!del]
  cat(sprintf("Unique preference curves: %d\n",NROW(diffs)))
  if(!is.na(ncol)) par(mfrow=c(ceiling(NROW(diffs)/ncol),ncol))
  plot(diffs,attribs=attrib)
  invisible(diffs)
}
