
get_report_ui <- function() {
  
  box(title = "Hent rapport",
             p("Titel"),
             textInput("report_title",label=NULL, value = "ESG Rapport"),
             p("Kernefortælling (valgfri)"),
             textAreaInput("report_intro", label=NULL),
             p("Fakta om bedriften (valgfri)"),
             textAreaInput("report_facts", label=NULL),
             downloadButton("generate_report", "Download PDF")
      )
}

get_report_server <- function() {

    output$generate_report <- downloadHandler(
      
      filename = function(){
        paste0(input$report_title,".pdf")
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "ESGReport.Rmd")
        file.copy("ESGReport.Rmd", tempReport, overwrite = TRUE)
        
        params_df <- NULL
        for(i in 1:length(names(input))){
          params_df <- as.data.frame(rbind(params_df,(cbind(names(input)[i],input[[names(input)[i]]]))))
        }
        names(params_df) <- c("input_name","input_value")
        title = input$report_title
        render_markdown <- function(){
          rmarkdown::render(tempReport,
                            output_file = file,
                            params = list(params_df,title)
                            #envir = new.env(parent = globalenv())
          )}
        render_markdown()
      }
    )
}
