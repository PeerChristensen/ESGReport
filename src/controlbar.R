

show_controlbar_items <- function() {
  
  div(
    column(12,
           div(style="text-align: center;margin-top: 25px;",
               actionButton("complete", "Skab rapport",width="150px")
           )
    ),
    column(12,
           div(style="text-align: center;margin-top: 25px;",
               actionButton("add_indicators", "Tilføj indikatorer",width="150px")
           )
    ),
    column(12,
           div(style="text-align: center;margin-top: 25px;",
               actionButton("save", "Gem rapport", width="150px")
           )
    ),
    column(12,
           div(style="text-align: center;margin-top: 250px;",
               actionButton("reset", "Start forfra",width="150px")
           )
    )
  )
}