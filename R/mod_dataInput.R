#' dataInput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

`%>%` <- dplyr::`%>%`
addResourcePath("d", "inst/app/www/")
mod_dataInput_ui <- function(id){
  ns <- NS(id)

  tagList(
#    mainPanel(
#     tabsetPanel(
    selectizeInput(ns("pathInput"), "Enter Path to Files:", options = list(create = TRUE), c(
      "/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/*/outs/per_sample_outs/*/count/sample_filtered_barcodes.csv" ,
      "/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/GEX_VDJ/*/outs/filtered_feature_bc_matrix/barcodes.tsv.gz",
      "/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/GEX_ADT/*/outs/filtered_feature_bc_matrix/barcodes.tsv.gz",
      "/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/2OUT_*_*/outs/filtered_feature_bc_matrix/barcodes.tsv.gz",
      "/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/2OUT_*_*/outs/filtered_feature_bc_matrix/aggregate_barcodes.csv",
      "/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/2OUT_BCR_?/outs/cell_barcodes.json",
      "/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/2OUT_ADT_?/outs/filtered_feature_bc_matrix/barcodes.tsv.gz",
      "/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/2OUT_TCR_?/outs/cell_barcodes.json",
      "/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_2/RUN/*/outs/per_sample_outs/*/count/sample_filtered_barcodes.csv"

    ), selected = character(0)),
    div(style="display: inline-block;horizontal-align:top; width: 15%;",textInput(ns("sub1"),label="Label1")),
    div(style="display: inline-block;horizontal-align:top; width: 15%;",textInput(ns("sub2"),label="Label2")),
    # withSpinner(actionButton("submitBtn", "Submit"))
    actionButton(ns("submitBtn"), "Submit")
    #)
  )

}


#' dataInput Server Functions
#'
#' @noRd
mod_dataInput_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$text<-renderText(
      input$pathInput
    )

    read_csv_filename <- function(filename){

      if(xfun::file_ext(filename) == 'json'){
        ret <- as.tibble(fromJSON(filename))
      }
      else{

        ret <- readr::read_csv(filename,col_names = FALSE) %>% dplyr::select(last_col())

      }

      #  ret <- read_csv(filename,col_names = FALSE) %>% select(last_col())

      # HTML("/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/2OUT_GEX_1/outs/filtered_feature_bc_matrix/barcodes.tsv.gz<BR>")

      colnames(ret) = c("bar")
      ret$file <- gsub(".*RUN/(.*)/outs.*", "\\1", filename)
      ret
    }

 #  inc<- observeEvent(input$submitBtn, {
      # withSpinner({


    userFile <- reactive({
      req(input$submitBtn)
      validate(need(input$pathInput !="", ""))
      ddd<-input$pathInput
      input$pathInput
    })



    ##   observeEvent(input$submitBtn, {
    datafile <- reactive({



   req(input$submitBtn)
      isolate({
      #
    #  inFile<-read_csv_filename()
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())

      progress$set(message = "Making plot", value = 0)



        path <- userFile()

       #path <- input$pathInput

      temp <- system(paste("ls ", path, sep = ""), intern = TRUE)
      data_list <- lapply(temp, read_csv_filename)
      result <- do.call(rbind, data_list)

      df <- result
      ddf<-result

      df$file =gsub(input$sub1,input$sub2,df$file)

      bar_lists <- split(df$bar, df$file)
  #    aaa<<-input$sub1



      # Initialize a matrix to store the intersection counts
      intersection_counts <- matrix(0, nrow = length(bar_lists), ncol = length(bar_lists),
                                    dimnames = list(names(bar_lists), names(bar_lists)))

      # Calculate intersections and populate the matrix
      for (i in 1:length(bar_lists)) {
        for (j in i:length(bar_lists)) {
          intersection_counts[i, j] <- length(intersect(bar_lists[[i]], bar_lists[[j]]))
          intersection_counts[j, i] <- intersection_counts[i, j]
        }
        progress$inc(1/length(bar_lists), detail = paste("Doing part", i))
        Sys.sleep(0.1)
        }



      return(
list(
          "intersection_counts" = intersection_counts
        )
)
      })
})





     return(list
            (subtn = reactive({input$submitBtn}),
              df = datafile )
     )



  })
}

