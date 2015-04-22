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

# Access Denied Message
no_access_message <- "Sorry, you don't have access to this app."

server <- function(input,output,session){
  
  # Load Permissions File (or query DB, etc.):
  permissions <- read.csv("permissions.csv",stringsAsFactors=FALSE)
  
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
      
      # For simple demonstration, username = user1 and password = pass1:
      if (username == "user1" & password == "pass1") {
        status <- 1
      } else {
        status <- 0
      }
      
      list(status = status
           ,username = username)
      
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
        
        # Load Permissions After User is Identified and Check against Table
        perm_vals$admin_access <- ifelse(subset(permissions,id==tolower(profile()$username))[,"admin_access"] == 1,TRUE,FALSE)
        perm_vals$app_1_access <- ifelse(subset(permissions,id==tolower(profile()$username))[,"app_1_access"] == 1,TRUE,FALSE)
        perm_vals$app_2_access <- ifelse(subset(permissions,id==tolower(profile()$username))[,"app_2_access"] == 1,TRUE,FALSE)
        perm_vals$app_3_access <- ifelse(subset(permissions,id==tolower(profile()$username))[,"app_3_access"] == 1,TRUE,FALSE)
      
        } else  {
        "Username or password failed!"
      }
    }
  })
  #####
  
  ##### Admin / Set Permissions:
  # Pass Permissions to ui.R as output values for conditional tabs
  perm_vals <- reactiveValues(admin_access = FALSE
                              ,app_1_access = FALSE
                              ,app_2_access = FALSE
                              ,app_3_access = FALSE)
  
  output$admin_access <- reactive({perm_vals$admin_access})
  output$app_1_access <- reactive({perm_vals$app_1_access})
  output$app_2_access <- reactive({perm_vals$app_2_access})
  output$app_3_access <- reactive({perm_vals$app_3_access})
  outputOptions(output, "admin_access", suspendWhenHidden=FALSE)  
  outputOptions(output, "app_1_access", suspendWhenHidden=FALSE)  
  outputOptions(output, "app_2_access", suspendWhenHidden=FALSE)  
  outputOptions(output, "app_3_access", suspendWhenHidden=FALSE)
  
  input_perm <- reactive({
    
    if(input$save_perm > 0){
      
      isolate({
        id <- input$id_perm
        admin_access <- input$admin_perm
        app_1_access <- input$app_1_perm
        app_2_access <- input$app_2_perm
        app_3_access <- input$app_3_perm
      })
      
      # Create new set of permissions for ID
      perms_new <- data.frame(id = tolower(id)
                              ,admin_access = admin_access
                              ,app_1_access = app_1_access
                              ,app_2_access = app_2_access
                              ,app_3_access = app_3_access
                              ,stringsAsFactors = FALSE)
      
      # Load old permissions
      perms_tmp <- read.csv("permissions.csv",stringsAsFactors=FALSE)
      
      # Look up ID in old permission table. If exists, update old record. Else, create new record.
      if (perms_new$id %in% perms_tmp[,"id"]){
        perms_tmp[which(perms_new$id == perms_tmp[,"id"]),"admin_access"] <- perms_new$admin_access
        perms_tmp[which(perms_new$id == perms_tmp[,"id"]),"app_1_access"] <- perms_new$app_1_access
        perms_tmp[which(perms_new$id == perms_tmp[,"id"]),"app_2_access"] <- perms_new$app_2_access
        perms_tmp[which(perms_new$id == perms_tmp[,"id"]),"app_3_access"] <- perms_new$app_3_access
      } else {
        row_count <- nrow(perms_tmp)
        perms_tmp[row_count+1,"id"] <- perms_new$id
        perms_tmp[row_count+1,"admin_access"] <- perms_new$admin_access
        perms_tmp[row_count+1,"app_1_access"] <- perms_new$app_1_access
        perms_tmp[row_count+1,"app_2_access"] <- perms_new$app_2_access
        perms_tmp[row_count+1,"app_3_access"] <- perms_new$app_3_access
      }
      
      # Write new permissions to file
      write.csv(perms_tmp,"permissions.csv",row.names=FALSE)
      return(perms_tmp)
    } else {
      
      # If nothing has changed, return the original permissions for reference
      return(permissions)
    }
    
  })
  
  output$perm_table <- renderTable({
    input_perm()
  })
  
}

ui <- navbarPage("App with User Auth and Admin"
                 
                 # Login Tab
                 ,tabPanel("Login"
                           ,conditionalPanel("output.status == null"
                                             ,h3(loading_message)
                           )
                           ,conditionalPanel("output.status == 0"
                                             ,sidebarPanel(p("Username = user1; Password = pass1")
                                                           ,htmlOutput("ui_login")
                                                           ,htmlOutput("pass")
                                             )
                           )
                           ,conditionalPanel("output.status == 1"
                                             ,br()
                                             ,h1(welcome_message)
                           )
                 )
                 
                 # Admin Tab
                 ,tabPanel("Admin"
                           ,conditionalPanel("output.status == null"
                                             ,h3(loading_message)
                           )
                           ,conditionalPanel("output.status == 0"
                                             ,h3(login_message)
                           )
                           ,conditionalPanel("output.status == 1"
                                    ,conditionalPanel("output.admin_access == 0"
                                                      ,h3(no_access_message)
                                    )
                                    ,conditionalPanel("output.admin_access == 1"           
                                             # Application title
                                             ,titlePanel("Admin")
                                             
                                             # Sidebar Layout
                                             ,sidebarLayout(
                                               sidebarPanel("Set Permissions"
                                                            ,textInput("id_perm","User ID")
                                                            ,selectInput("admin_perm"
                                                                         ,label = "Admin"
                                                                         ,choices = list("No" = 0
                                                                                         ,"Yes" = 1))
                                                            ,selectInput("app_1_perm"
                                                                         ,label = "Application 1"
                                                                         ,choices = list("No" = 0
                                                                                         ,"Yes" = 1))
                                                            ,selectInput("app_2_perm"
                                                                         ,label = "Application 2"
                                                                         ,choices = list("No" = 0
                                                                                         ,"Yes" = 1))
                                                            ,selectInput("app_3_perm"
                                                                         ,label = "Application 3"
                                                                         ,choices = list("No" = 0
                                                                                         ,"Yes" = 1))
                                                            ,actionButton("save_perm", label = h6("Save"))
                                               )
                                               
                                               ,mainPanel(
                                                 tabPanel("View Permissions"
                                                          ,tableOutput("perm_table")
                                                          
                                                 )
                                                 
                                               )
                                             )
                                    )
                           )
                 )
                 
                 # App 1
                 ,tabPanel("App 1"
                           ,conditionalPanel("output.status == null"
                                             ,h3(loading_message)
                           )
                           ,conditionalPanel("output.status == 0"
                                             ,h3(login_message)
                           )
                           ,conditionalPanel("output.status == 1"
                                    ,conditionalPanel("output.app_1_access == 0"
                                                      ,h3(no_access_message)
                                    )
                                    ,conditionalPanel("output.app_1_access == 1"                                               
                                             # Application title
                                             ,titlePanel("App 1")
                                             
                                             # Sidebar Layout
                                             ,sidebarLayout(
                                               sidebarPanel(
                                                 
                                                 # Input Stuff
                                               )
                                               
                                               ,mainPanel(
                                                 # Output Stuff
                                                          
                                                 )
                                                 
                                               )
                                    )
                           )
                  )
                 # App 2
                 ,tabPanel("App 2"
                           ,conditionalPanel("output.status == null"
                                             ,h3(loading_message)
                           )
                           ,conditionalPanel("output.status == 0"
                                             ,h3(login_message)
                           )
                           ,conditionalPanel("output.status == 1"
                                    ,conditionalPanel("output.app_2_access == 0"
                                                      ,h3(no_access_message)
                                    )
                                    ,conditionalPanel("output.app_2_access == 1"                     
                                             # Application title
                                             ,titlePanel("App 2")
                                             
                                             # Sidebar Layout
                                             ,sidebarLayout(
                                               sidebarPanel(
                                                 
                                                 # Input Stuff
                                               )
                                               
                                               ,mainPanel(
                                                 # Output Stuff
                                                 
                                               )
                                               
                                             )
                                    )
                           )
                 )

                 # App 3
                 ,tabPanel("App 3"
                           ,conditionalPanel("output.status == null"
                                             ,h3(loading_message)
                           )
                           ,conditionalPanel("output.status == 0"
                                             ,h3(login_message)
                           )
                           ,conditionalPanel("output.status == 1"
                                    ,conditionalPanel("output.app_3_access == 0"
                                                      ,h3(no_access_message)
                                    )
                                    ,conditionalPanel("output.app_3_access == 1"              
                                             # Application title
                                             ,titlePanel("App 3")
                                             
                                             # Sidebar Layout
                                             ,sidebarLayout(
                                               sidebarPanel(
                                                 
                                                 # Input Stuff
                                               )
                                               
                                               ,mainPanel(
                                                 # Output Stuff
                                                 
                                               )
                                               
                                             )
                                    )
                           )
                 )
)

shinyApp(server=server,ui=ui)
