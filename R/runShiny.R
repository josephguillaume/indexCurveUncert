runShiny<-function(...){
    shiny.dir=system.file("interface", package = "indexCurveUncert")
    runApp(shiny.dir,...)
}
