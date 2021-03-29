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
  windowTitle = "Subgroup discovery",
  
  #icon("cog", lib = "glyphicon"),
  # Application title
  titlePanel("Subgroup explorer"),
  tags$head(tags$script(src = "message-handler.js")),
  
  # Sidebar for user inputs 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h2("Inputs"),
      
      fileInput(inputId = "fileIn", 
                label = "Select a file with Rules", 
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                           ),
     # hr(),
      selectizeInput('dropdown_subsets','Subsets', c('Initial Data'), options = NULL, width = NULL),
      #hr(),
      fluidRow(
        column(4,actionButton("btUpload", "Use selected file",icon("file-upload"), width = "100%")),
        column(4,actionButton("do", "Drop filter", icon("undo") , width = "100%")),
        column(4,actionButton("btSaveSubset", "Save Subset", icon("table") , width = "100%"))
      ),
      hr(),
      textInput("txt_filter", "Filter by String", value = ""),
      
      # Scatter Axis
      fluidRow(
        column(6, selectInput('scatterX','X-axis', supconflif, selected = "support")),
        column(6,selectInput('scatterY','Y-axis', supconflif, selected = "confidence"))
        ),
      #Scatter plot
      fluidRow(
        column(12,
               plotlyOutput("Scatter")
               )
        ),
      fluidRow(
          column(12,
               plotOutput("TargetBox", 
                            brush = brushOpts(id = "plot1_brush"),
                            click = "click_bar"
                            )
               )
      ),
    ),
    
  # MainPanel for viz
  
    mainPanel(
      width=9,
      verbatimTextOutput("ruleCount"),
    

  # tabset for different vizs
        tabsetPanel(
          tabPanel(title = "Tree",
            fluidRow(
              column(3,
                    fluidRow(
                      verbatimTextOutput("treeSummary")

                      ),
                    
                    
                    bucket_list(
                      header = "Use Drag-n-Drop to build the hierarchy",
                      add_rank_list(
                        text = "Drag from here",
                        labels = tail(items, 6)
                        
                      ),
                      add_rank_list(
                        text = "to hierarchy",
                        labels = items[1:2],
                        input_id = "bucketLHS"
                      )
                    )
                    ),
              column(9,  collapsibleTreeOutput("collapsTree"))
            )
          ), 
          
          ####### END TAB PANEL #######  
          tabPanel(title = "DecesionTree",  
                   fluidRow(
                     column(6, style = "background-color:#8ad3ff;",
                            h2("Regression Tree", align = "center"),
                            plotOutput("decisionTreeRegression"),
                            br()
                            ),
                     column(6, style = "background-color:#b6fffd;",
                            h2("Classification Tree", align = "center"),
                            plotOutput("decisionTreeClass"),
                            br()
                            )
                   )
                   
                   
                   ),
          
          
          ####### END TAB PANEL #######  
          
          tabPanel(title = "Data table",
                   DT::dataTableOutput("fileTable2"),
                   DT::dataTableOutput("tableSelected"),
                   actionButton('btFilterTable', label = "Filter by selected rules", icon = icon("exchange"),class= "btn-success" )
          ),
          
          ####### END TAB PANEL #######  
         
          tabPanel(title = "Info", 
                   h2('Explaination'),
                   p('The tool was developed as a practical part of Benjamin Wuthe\'s master\'s thesis in the Business Informatics program at HTW Berlin.'),
                   hr(),
                   h3('What is'),
                   p('Subgroup explorer'),
                   h3('How to use'),
                   p('Lorem Ipsum')
                   )
          
          ####### END TAB PANEL #######  
          
          
        )
      
    )
  )
)