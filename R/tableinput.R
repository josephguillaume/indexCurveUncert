#' Matrix input
#'
#' Creates an adjustable-length matrix input.
#'
#' @param inputId Input variable to assign the control's value to.
#' @param label Display label for the control.
#' @param data The initial values to use for the matrix.
#'
#' @export
matrixInput <- function(inputId, label, data,types=NULL) {
  addResourcePath(
    prefix='tableinput',
                  ## TODO: shouldn't be hardcoded
    directoryPath=system.file("interface", package = "indexCurveUncert"))

  if(is.null(types)) {
      type.lookup <- c("double"="numeric","integer"="integer",
                       "character"="character","logical"="logical")
      cat(sapply(names(data),function(name) typeof(data[[name]])),"\n",file=stderr())
      ##stopifnot(all(sapply(names(data),function(name) typeof(data[[name]]) %in% names(type.lookup))))
      types <- sapply(names(data),function(name) type.lookup[[typeof(data[[name]])]])
  }
  names(types) <- names(data)

  ##FIXME: this causes problems when called from server?
  ##FIXME: one row is added PER TABLE
  tagList(
    singleton(
      tags$head(
        tags$link(rel = 'stylesheet',
                  type = 'text/css',
                  href = 'tableinput/tableinput.css'),
        tags$script(src = 'tableinput/tableinput.js')
      )
    ),

    tags$div(
      class = 'control-group tableinput-container',
      tags$label(
        class = "control-label",
                 'for' = inputId, ##added to help selection of this element, and be consistent with other labels used
        label,
        tags$div(
          class = 'tableinput-buttons',
          tags$button(
            type = 'button', class = 'btn btn-mini tableinput-settings hide',
            tags$i(class = 'icon-cog')
          ),
          HTML('<a href="#" class="tableinput-plusrow"><i class="icon-plus-sign"></i></a>'),
          HTML('<a href="#" class="tableinput-minusrow"><i class="icon-minus-sign"></i></a>')
        )
      ),
      tags$table(
        id = inputId,
        class = 'tableinput data table table-bordered table-condensed',
        tags$colgroup(
          lapply(names(data), function(name) {
            tags$col('data-name' = name,
                     'data-field' = name,
                     'data-type' = types[[name]])
          })
        ),
        tags$thead(
          ##class = 'hide',
          tags$tr(
            lapply(names(data), function(name) {
              tags$th(name)
            })
          )
        ),
        tags$tbody(
          lapply(1:nrow(data), function(i) {
            tags$tr(
              lapply(names(data), function(name) {
                tags$td(
                  div(tabindex=0, as.character(data[i,name]))
                )
              })
            )
          })
        )
      )
##       tags$div(
##         class = 'tableinput-editor modal hide fade',
##         tags$div(
##           class = 'modal-header',
##           HTML('<button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>'),
##           tags$h3(label)
##         ),
##         tags$div(
##           class = 'modal-body',
## TODO: form breaks wellPanel formatting
##           HTML('
## <form class="form-horizontal">
##   <div class="control-group">
##     <label class="control-label">Rows</label>
##     <div class="controls">
##       <input type="number" class="tableinput-rowcount">
##     </div>
##   </div>
##   <div class="control-group">
##     <label class="control-label">Columns</label>
##     <div class="controls">
##       <input type="number" class="tableinput-colcount">
##     </div>
##   </div>
## </form>'
##           )
##         ),
##         tags$div(
##           class = 'modal-footer',
##           tags$a(href = '#', class = 'btn btn-primary tableinput-edit', 'OK'),
##           tags$a(href = '#',
##                  class = 'btn',
##                  'data-dismiss' = 'modal',
##                  'aria-hidden' = 'true',
##                  'Cancel')
##         )
##       )
    )
  )
}
