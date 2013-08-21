runShiny<-function(...){
    library(shiny)
    shiny.dir=system.file("interface", package = "indexCurveUncert")
    runApp(shiny.dir,...)
}
