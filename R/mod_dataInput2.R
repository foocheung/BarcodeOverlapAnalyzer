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
mod_dataInput_ui2 <- function(id){
  ns <- NS(id)

  tagList(


    tabsetPanel(
      tabPanel("Intersection Counts Heatmap",

HTML("<BR>"),

               div(style="display: inline-block;vertical-align:top; margin-left: 80px; width: 15%;",selectInput(ns("norm"), "Normalization", c("normalize", "percentize", "scale"), selected = "normalize")),
               div(style="display: inline-block;horizontal-align:top; width: 15%;",sliderInput(ns("width"), "Plot Width:",
                                                                                               min = 400, max = 2000, value = 600)) ,
               div(style="display: inline-block;horizontal-align:top; width: 15%;",sliderInput(ns("height"), "Plot Height:",
                                                                                               min = 400, max = 2000, value = 600)),
               plotly::plotlyOutput(ns("heatmap")),
               textOutput(ns("text")),
      ),
      tabPanel("Counts Table", DT::DTOutput(ns("table"))),
      tabPanel("Urls",
               HTML("/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/GEX_VDJ/outs/filtered_feature_bc_matrix/barcodes.tsv.gz<BR>"),
               HTML("/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/GEX_ADT/outs/filtered_feature_bc_matrix/barcodes.tsv.gz<BR>"),
               HTML("/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/23_306_3*/outs/per_sample_outs/*/count/sample_filtered_barcodes.csv<BR>"),
               HTML("/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/2OUT_GEX_?/outs/filtered_feature_bc_matrix/barcodes.tsv.gz<BR>"),
               HTML("/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/2OUT_BCR_?/outs/cell_barcodes.json<BR>"),
               HTML("/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/2OUT_ADT_?/outs/filtered_feature_bc_matrix/barcodes.tsv.gz<BR>"),
               HTML("/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/2OUT_TCR_?/outs/cell_barcodes.json<BR>"),
               HTML("/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_2/RUN/*/outs/per_sample_outs/*/count/sample_filtered_barcodes.csv<BR>"),
               HTML("/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/*/outs/per_sample_outs/*/count/sample_filtered_barcodes.csv<BR>"),
               HTML("/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/Manthiram_Covid-tonsil_CHI-306/NEW_DATA_9_sept_2023/230908_A00941_1368_AHKVGVDSX2/RUN_23_306_3/RUN/2OUT_*_?/outs/cell_barcodes.json<BR>")

      ))
  )
}


#' @noRd
#' @export
#' @keywords internal
#'

mod_dataInput_server2 <- function(input, output, session, file){  #,batches,sim){
  ns <- session$ns

    output$heatmap <-  plotly::renderPlotly({
  req(file$subtn())
        in1<-file$df()$intersection_counts
    in2<<-in1

    ## if(input$sub1){
    #  df$file =gsub(input$sub1,input$sub2,df$file)
    # }

      if (input$norm == "normalize") {
        heatmaply::heatmaply(heatmaply::normalize(in1), cellnote_textposition="middle center", cellnote = in1, plot_method = "ggplot",fontsize = 7, width = input$width, height = input$height )
      }
      else if(input$norm == "percentize"){
        heatmaply::heatmaply(heatmaply::percentize(in1),  cellnote_textposition="middle center", cellnote = in1,plot_method = "ggplot",fontsize = 7, width = input$width, height = input$height )
      }
      else if(input$norm == "scale"){
        #  heatmaply::heatmaply(scale="column",(t(as.matrix(as.data.frame(d)))), plot_method="plotly",labRow=rownames(d) ,row_side_colors =rownames(d), width = input$width, height = input$height )
        heatmaply::heatmaply(scale="none",log(in1 +1), cellnote_textposition="middle center", cellnote = in1 +1,  plot_method = "ggplot",fontsize = 7, width = input$width, height = input$height )

      }



  })



}
