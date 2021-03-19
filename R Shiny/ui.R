library(shinythemes)
library(plotly)
library(sortable)
library(r2d3 )
library(collapsibleTree)


items <- c("AGE","MODEL",	"DEALERSHIP"	,"CUSTOMER_TYPE",	"USER_CUSTOMISED",
           "COUNTRY",	"GEO_TYPE",	"FAULT_TYPE")
supconflif <- c('support', 'confidence','lift')
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Subgroup explorer"),
  tags$head(tags$script(src = "message-handler.js")),
  
  # Sidebar for user inputs 
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
      
      
      
      fluidRow(
        column(6, selectInput('scatterX','X-axis', supconflif, selected = "support")),
        column(6,selectInput('scatterY','Y-axis', supconflif, selected = "confidence"))
        ),
      
      br(''),
      
      # sliderInput( 
      #   "slSupp",
      #   "Min Support:",
      #   min = 0,
      #   max = 1,
      #   value = 0),
      # 
      # sliderInput( 
      #   "slConf",
      #   "Min Confidence:",
      #   min = 0,
      #   max = 1,
      #   value = 0),
      # 
      # sliderInput( 
      #   "slLift",
      #   "Min Lift:",
      #   min = 0,
      #   max = 25,
      #   value = 0)
    
      
    ),
    
  # MainPanel for viz
  
    mainPanel(
      width=9,
      verbatimTextOutput("ruleCount"),
      fluidRow(
        column(6,
          plotOutput("TargetBox", brush = brushOpts(
            id = "plot1_brush"
          ),
          click = "click_bar"),
        ),
        column(6,
          plotlyOutput("Scatter")
          )
        
      ),
      

  # tabset for different vizs
        tabsetPanel(
          tabPanel(title = "Main Info",
            fluidRow(
              column(3,
                    fluidRow(
                      verbatimTextOutput("treeSummary")

                      ),
                    
                    
                    bucket_list(
                      header = "Use Drag-n-Drop to build the hierarchy",
                      add_rank_list(
                        text = "Drag from here",
                        labels = items
                        
                      ),
                      add_rank_list(
                        text = "to hierarchy",
                        labels = NULL,
                        input_id = "bucketLHS"
                      )
                    )
                    ),
              column(9,  collapsibleTreeOutput("collapsTree"))
            )
          ), 
          
          ####### END TAB PANEL #######  
          tabPanel(title = "DecesionTree",  plotOutput("decisionTree")),
          
          
          ####### END TAB PANEL #######  
          
          tabPanel(title = "Data table2",
                   DT::dataTableOutput("fileTable2"),
                   DT::dataTableOutput("tableSelected"),
                   actionButton('btFilterTable', label = "Filter by selected rules", icon = icon("exchange"),class= "btn-success" )
          ),
          
          ####### END TAB PANEL #######  
         
          tabPanel(title = "D3 test", d3Output("d3_test"))
          
          ####### END TAB PANEL #######  
          
          
        )
      
    )
  )
)