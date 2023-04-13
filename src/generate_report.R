
get_report_ui <- function() {
  
  box(width=9,
      title = "Skab rapport",
      p("Titel"),
      textInput("report_title",label=NULL, value = "ESG Rapport"),
      p("Kernefortælling"),
      textAreaInput("report_intro", label=NULL),
      p("Fakta om bedriften"),
      textAreaInput("report_facts", label=NULL,
                    placeholder = "Separér fakta med '-'  (Eksempel: - Faktum 1 - Faktum 2..)"
                    ),
      splitLayout(
        cellWidths = c("40%", "40%"),
        downloadButton("generate_report", "Download PDF"),
        downloadButton("generate_report_html", "Download HTML"),
        align = "center",
        style = "margin-top: 50px;"
        )
      )
}

get_report_server <- function(input, indicator_info_df) {

  downloadHandler(
    filename =  "ESGreenToolReport.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "ESGReport.Rmd")
      tempCSS <- file.path(tempdir(), "report_style.css")
      file.copy("templates/ESGReport.Rmd", tempReport, overwrite = TRUE)
      file.copy("www/report_style.css", tempCSS, overwrite = TRUE)
      params <-   reactiveValuesToList(input)
      html_fn <- rmarkdown::render(tempReport,  
                                   params = list(
                                     "params" = params,
                                     "indicators_df" = indicator_info_df
                                   ),
                                   envir = new.env(parent = globalenv()))
      
      pagedown::chrome_print(html_fn, file, extra_args = c("--no-sandbox"))
    }
  )
}

get_report_server_html <- function(input, indicator_info_df) {
  downloadHandler(
    filename =  "ESGreenToolReport.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "ESGReport_html.Rmd")
      tempCSS <- file.path(tempdir(), "report_style.css")
      file.copy("templates/ESGReport_html.Rmd", tempReport, overwrite = TRUE)
      file.copy("www/report_style.css", tempCSS, overwrite = TRUE)
      params <-   reactiveValuesToList(input)
      
      render_markdown <- function(){
        rmarkdown::render(tempReport,
                          output_file = file,
                          params = list(
                            "params" = params,
                            "indicators_df" = indicator_info_df
                          ),
                          envir = new.env(parent = globalenv())
        )}
      render_markdown()
    }
  )
}

