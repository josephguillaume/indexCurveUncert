weight.classes <- function(x){
    library(rcdd)
    nattrib <- length(x)
    ws.all <- NULL
    for(dir in c(-1,1)){ ##-1 >=, 1<=
        constr <- list(constr=
                       matrix(byrow=T,ncol=nattrib,c(
                                      rep(1,nattrib), ##Sum to 1
                                      ##-x,             ##dS>=0
                                      dir*x, ##dS<0
                                      -diag(1,nrow=nattrib,ncol=nattrib), ## >=0
                                      diag(1,nrow=nattrib,ncol=nattrib) ## <=1
                                      )),
                       rhs=c(1,0,rep(0,nattrib),rep(1,nattrib)),
                       dir=c("=","<=",rep("<=",nattrib),rep("<=",nattrib))
                       )

        h <- makeH(constr$constr[constr$dir=="<=",],constr$rhs[constr$dir=="<="],
                   constr$constr[constr$dir=="=",],constr$rhs[constr$dir=="="],
                   )
        v <- q2d(scdd(d2q(h))$output)
        validcdd(h)
        validcdd(v,representation="V")
        stopifnot(v[, 1] == "0")
        stopifnot(v[, 2] == "1")
        ws.all <- rbind(ws.all,cbind(dir=dir,
                                     v[,-(1:2)]))
    }
    colnames(ws.all) <- c("dir",sprintf("w%d",1:nattrib))
    rownames(ws.all) <- 1:nrow(ws.all)
    ws.all
}

plot.weight.classes <- function(x,w.vertices,current.weights=NULL){
    if(missing(w.vertices)) w.vertices <- weight.classes(x)
    nattrib <- length(x)
    par(mfrow=c(nattrib,nattrib))
    cc <- expand.grid(1:nattrib,1:nattrib)
    for(i in 1:nrow(cc)){
        ws.pos <- w.vertices[w.vertices[,1]==-1,-1]
        ws.neg <- w.vertices[w.vertices[,1]==1,-1]
        ch.pos <- chull(ws.pos)
        ch.pos <- c(ch.pos,ch.pos[1])
        ch.neg <- chull(ws.neg)
        ch.neg <- c(ch.neg,ch.neg[1])
        if(cc[i,1]==cc[i,2]) {
            plot(NULL,xlim=c(0,1),ylim=c(-1,1),
                 xlab=sprintf("w%d",cc[i,1]),ylab=NA
                 )
            segments(x0=min(ws.pos[,cc[i,1]]),x1=max(ws.pos[,cc[i,1]]),y0=1,y1=1,col="green",lwd=2)
            segments(x0=min(ws.neg[,cc[i,1]]),x1=max(ws.neg[,cc[i,1]]),y0=-1,y1=-1,col="red",lwd=2)
            if(!is.null(current.weights)) abline(v=current.weights[cc[i,1]])
        } else {
            ##plot(w.vertices[ch,cc[i,1]],w.vertices[ch,cc[i,2]],type="l",xlim=c(0,1),ylim=c(0,1))
            plot(NULL,xlim=c(0,1),ylim=c(0,1),
                 xlab=sprintf("w%d",cc[i,1]),ylab=sprintf("w%d",cc[i,2])
                 )
            polygon(ws.neg[ch.neg,as.numeric(cc[i,])],col="red")
            polygon(ws.pos[ch.pos,as.numeric(cc[i,])],col="green")
            if(!is.null(current.weights)) points(current.weights[cc[i,1]],current.weights[cc[i,2]])
        }
    }

}

## weight.comp <- data.frame(a="gwlevel",
##                           b="test",
##                           status="<=",min.gap=0.05)
## weight.bounds <- data.frame(a="duration",min=NA,max=0)

## $constr
##    a b status min.gap
## 1 NA 1     <=   -0.05
## $bounds
## $bounds$lower
## [1] 0
## $bounds$upper
## [1] 1

## $constr
##            [,1]       [,2]       [,3]
## [1,]  1.0000000  1.0000000  1.0000000
## [2,]  0.9982262  0.9401245 -0.5324055
## [3,] -1.0000000  0.0000000  0.0000000
## [4,]  0.0000000 -1.0000000  0.0000000
## [5,]  0.0000000  0.0000000 -1.0000000
## [6,]  1.0000000  0.0000000  0.0000000
## [7,]  0.0000000  1.0000000  0.0000000
## [8,]  0.0000000  0.0000000  1.0000000
## $rhs
## [1] 1 0 0 0 0 1 1 1
## $dir
## [1] "="  "<=" "<=" "<=" "<=" "<=" "<=" "<="
