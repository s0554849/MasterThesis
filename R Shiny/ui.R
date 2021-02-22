library(shinythemes)

library(sortable)
items <- c("AGE","MODEL",	"DEALERSHIP"	,"CUSTOMER_TYPE",	"USER_CUSTOMISED",
           "COUNTRY",	"GEO_TYPE",	"FAULT_TYPE")

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Subgroup explorer"),
  tags$head(tags$script(src = "message-handler.js")),
  
  # Sidebar for user inputs 
  sidebarLayout(
    sidebarPanel(
      h2("Inputs"),
      
      fileInput(inputId = "fileIn", label = "Select a file with Rules", 
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
      ),
      actionButton("btUpload", "Use selected file",icon("file-upload"), class= "btn-success"),
      
      column(6, actionButton("do", "Show Msg")),
      
      textInput("txt_filter", "Filter by String", value = ""),
      fluidRow(
        column( 12,
                fluidRow(
                  column(6,actionButton("rv1s", "Data set 1 save")),
                  column(6,actionButton("rv1l", "Data set 1 load")),
                  )
                )
        ),
      br(''),
      fluidRow(
        column( 12,
                fluidRow(
                  column(6,actionButton("rv2s", "Data set 2 save")),
                  column(6,actionButton("rv2l", "Data set 2 load"))
                )
        )
      ),
      br(''),
      
      sliderInput( 
                  "slSupp",
                  "Min Support:",
                  min = 0,
                  max = 1,
                  value = 0),
      
      
      

      bucket_list(
        header = "This is a bucket list. You can drag items between the lists.",
        add_rank_list(
          text = "Drag from here",
          labels = items
        ),
        add_rank_list(
          text = "to here LHS",
          labels = NULL
        ),
        add_rank_list(
          text = "to here RHS",
          labels = NULL
        )
      )
    
      
    ),
    
  # MainPanel for viz
  
    mainPanel( 
      verbatimTextOutput("ruleCount"),
      plotlyOutput("TargetBox"),
  # tabset for different vizs
        tabsetPanel(
          #tabPanel(title = "textout", verbatimTextOutput("binsNum"),
          tabPanel(title = "Main Info", plotOutput("distPlot")),
          tabPanel(title = "Data table", tableOutput("fileTable")),
          tabPanel(title = "Figure 3", plotOutput("distPlot3"))
          
        )
      
    )
  )
)