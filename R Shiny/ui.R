library(shinythemes)
library(sortable)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Subgroup explorer"),
  
  # Sidebar for user inputs 
  sidebarLayout(
    sidebarPanel(
      h2("Inputs"),
      sliderInput( 
                  "bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      fileInput(inputId = "fileIn", label = "Select a file with Rules", 
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
                )
    
      
    ),
    
  # MainPanel for viz
  
    mainPanel( 
  # tabset for different vizs
      tabsetPanel(
        #tabPanel(title = "textout", verbatimTextOutput("binsNum"),
        tabPanel(title = "Figure 1", plotOutput("distPlot")),
        tabPanel(title = "Figure 2", tableOutput("fileTable")),
        tabPanel(title = "Figure 3", plotOutput("distPlot3"))
        
      )
      
    )
  )
)