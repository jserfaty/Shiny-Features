# See: http://withr.me/authentication-of-shiny-server-application-using-a-simple-method/
if (!require(shiny))
  install.packages(shiny)
# For Verifying Credentials via API
#if (!require(RCurl))
#  install.packages(RCurl)
#if (!require(XML))
#  install.packages(XML)

# Application Loading Message
loading_message <- "Loading..."

# Authentication Message
login_message <- "Please log in."

# Welcome Message
welcome_message <- "Welcome!"

server <- function(input,output,session){
  
  ##### User Authentication:
  user <- reactiveValues(logged = FALSE)
  output$status <- reactive({user$logged})
  outputOptions(output, "status", suspendWhenHidden=FALSE)
  
  # Login Fields
  output$ui_login <- renderUI({
    wellPanel(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      br(),
      actionButton("login", "Log in"),
      a(href = "http://shiny.rstudio.com/", target="_blank", "Forgot your password?")
    )
  })
  
  # Authenticate User
  profile <- reactive({
    if (input$login > 0) {
      
      username <- isolate(input$username)
      password <- isolate(input$password)
      
      # For simple demonstration, username = 1 and password = 1:
      if (username == 1 & password == 1) {
        status <- 1
      } else {
        status <- 0
      }
      
      list(status = status)
      
# Optionally, Post Credentials to API for Verification
#       url <- "www.example.com"
#       postContent <- URLencode(paste0("id=",username,"&password=",password))
#       myHeader <- c('Content-Type'  = "application/x-www-form-urlencoded")
#       resp <-  getURL(url             = url
#                       ,postfields     = postContent
#                       ,httpheader     = myHeader
#                       ,verbose        = TRUE)
#       
#       # Parse Response and Extract Valuable Info
#       resp_parsed <- xmlToList(resp)
#       
#       list(status = resp_parsed$status
#            ,id = resp_parsed$id
#            ,email = resp_parsed$email)
      
    } 
  })
  
  # Check Username and Password
  output$pass <- renderText({  
    if (input$login > 0) {
      
      if (profile()$status == 1) {
        user$logged <- TRUE
      } else  {
        "Username or password failed!"
      }
    }
  })

  
}

ui <- navbarPage("User Authentication"

                 ,tabPanel("Login"
                           ,conditionalPanel("output.status == null"
                                             ,h3(loading_message)
                           )
                           ,conditionalPanel("output.status == 0"
                                             ,sidebarPanel(p("Username = 1; Password = 1")
                                                          ,htmlOutput("ui_login")
                                                          ,htmlOutput("pass")
                                             )
                           )
                           ,conditionalPanel("output.status == 1"
                                             ,br()
                                             ,h1(welcome_message)
                           )
                 )
                 
                 # Tab 1
                 ,tabPanel("Tab 1"
                           ,conditionalPanel("output.status == null"
                                             ,h3(loading_message)
                           )
                           ,conditionalPanel("output.status == 0"
                                             ,h3(login_message)
                           )
                           ,conditionalPanel("output.status == 1"
                                             
                                             # Application title
                                             ,titlePanel("Application 1")
                                             
                                             # Sidebar Layout
                                             ,sidebarLayout(
                                               sidebarPanel(
                                                 
                                                 # Input Stuff
                                                 
                                               )
                                               
                                               ,mainPanel(
                                                 tabsetPanel(
                                                   tabPanel("Panel 1"
                                                            
                                                            # Output Stuff
                                                   )
                                                 )
                                               )
                                             )
                           )
                 )
)

shinyApp(server=server,ui=ui)