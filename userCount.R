# See: https://gist.github.com/trestletech/9926129
if (!require(shiny))
  install.packages(shiny)

# For Current User Count:
vals <- reactiveValues(count=0)

server <- function(input,output,session){
  
  # when a session begins, add 1 to count
  isolate(vals$count <- vals$count + 1)
  
  # when a session ends, subtract 1 from count
  session$onSessionEnded(function(){
    isolate(vals$count <- vals$count - 1)
  })
  
  # Put count in an output object to display in ui.R
  output$count <- renderText({
    vals$count
  })

}

ui <- navbarPage("How Many People Are Using the App?"
  # Show How Many People are Using the App
  ,header = div(p("There are currently ",textOutput("count",inline=TRUE)," app users."))
)

shinyApp(server=server,ui=ui)
