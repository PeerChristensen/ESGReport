
get_report_ui <- function() {
  
  box(title = "Hent rapport",
             p("Titel"),
             textInput("report_title",label=NULL, value = "ESG Rapport"),
             p("Kernefortælling (valgfri)"),
             textAreaInput("report_intro", label=NULL,
                           value = "For LaTeX output, the first class name will be used as the LaTeX environment name. You should also provide an attribute named data-latex in the Div block, which will be the arguments of the environment. This attribute can be an empty string if the environment does not need arguments. We show two simple examples below. The first example uses the verbatim environment in LaTeX, which does not have any arguments:"),
             p("Fakta om bedriften (valgfri)"),
             textAreaInput("report_facts", label=NULL,
                           value = "- For LaTeX output, the first class name will be used as the LaTeX environment name - We show two simple examples below. The first example uses the verbatim environment in LaTeX, which does not have any arguments - This attribute can be an empty string if the environment does not need arguments."),
             downloadButton("generate_report", "Download PDF"),
             downloadButton("generate_report_html", "Download HTML")
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
