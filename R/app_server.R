#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  metafile <- mod_dataInput_server("dataInput_ui")

  metafile2 <- callModule(mod_dataInput_server2,"dataInput_ui2",metafile)

}
