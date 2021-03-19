# TODO

# Button und 
# Filter

interesting_cols <- c("FAULT_TYPE","rules","support","confidence","lift",
                      "FAULT_COUNT","MODEL","AGE","DEALERSHIP","CUSTOMER_TYPE",
                      "USER_CUSTOMISED", "COUNTRY","GEO_TYPE", "OBJECT_COUNT", 
                      "FAULT_RATE",  "AGGREGATION_LEVEL", "SET_SIZE" )

supconflif <- c('support', 'confidence','lift')
#selectedHier <-c("MODEL", "AGE", "FAULT_TYPE")

library(plotly)
library(ggplot2)

library(dplyr)

library(collapsibleTree)

library(rpart)
library(rpart.plot)


df <- read.csv("/Users/benjaminwuthe/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Joshuas stuff/Daten/SCHRITT_4_1_MINSUP_MINCONF.csv")[,interesting_cols]

treeSummaryText <- ""

server <- function(input, output, session) {
  print("Start")
  
  #data()

  
  # Observer Function- to update all views
  observeEvent(input$do, {
    session$sendCustomMessage(
      type = 'testmessage',
      message = 'Thank you for clicking')
  })
rv <-reactiveValues()
data <-  observeEvent(input$btUpload, {
    print("BT clicked")
    df <<- read.csv(input$fileIn$datapath,sep = ",")[,interesting_cols]
    rv$data <- df
  })

  observeEvent(input$click_bar,{
    print(nearPoints(df_cnts, input$plot1_click, addDist = TRUE))
  })
  
  observeEvent(input$plot1_brush,{
    print(input$plot1_brush)
    brushedPoints(df_cnts, input$plot1_brush)
    #print("brush on barchart")
  })
  
  # data$x <-input$scatterX
  # xyzDims <- reactive({
  #   x<-input$scatterX
  #   y<-input$scatterY
  #   z<-supconflif[supconflif != c(x,y)]
  #   print(x,y,z)
  #   c(x,y , z )
  # })
  
  xyzArr <- reactiveValues()
  
  xyzArr$x = "support"
  xyzArr$y = "confidence"
  xyzArr$z = "lift"
  
  getZAxis <- function(x,y) {
    # returns missing
    if (x!=y){
      print(supconflif[supconflif != c(x, y)])
      supconflif[supconflif != c(x, y)]
    }
  }
  
  observeEvent(input$scatterX,{
    xyzArr$x = input$scatterX
  })
  
  observeEvent(input$scatterY,{
    xyzArr$y = input$scatterY
  })
  
  output$Scatter <- renderPlotly(
    {
      # Get x and y axis, and find last part from supp conf lift in zaxis
      axs <- xyzArr
      
      

      #print(paste("X: ", xaxis, " Y: ", yaxis, " Z: ", zaxis))
      z <- getZAxis(axs$x,axs$y)
        
      d <-as.data.frame( rv$data)
      layout( plot_ly(data = d , x = d[,axs$x], y = d[,axs$y], color = d[,axs$z]), 
              
              xaxis = list(title= axs$x),
              yaxis = list(title= axs$y)
              
              )
      
      # fig <- fig %>% layout()
      #~FAULT_TYPE)
    }
  )
  
  # Ovserve all actions
  # observe({
  #   print("Observer listening")
  #   # Read CSV if changes
  #   if (  is.null(input$fileIn$datapath)){
  #     print("Observer filpath is null")
  #   }
  #   else{
  #     # New file selected
  #     print("Observer filpath is NOT null")
  #   }
  # 
  # })

  # data <- reactive(input$do,{
  #   if (is.null(input$fileIn$datapath)){
  #     print("Observer filpath is null")
  #   }
  #   else{
  #     # New file selected
  #     print("Observer filpath is NOT null")
  #     df <<- read.csv(input$fileIn$datapath,
  #                     sep = ",")
  #     df
  # 
  #   }
  # })
  
  # Filter data by txt input
  data <- eventReactive(input$txt_filter,{
      txt <- input$txt_filter
      if (txt!=""){
        txtlist <- strsplit(txt, ';')

        for (i in 1:length(txtlist[[1]])){
          filt <-txtlist[[1]][i]
          print(paste("Filter ", filt))
          df <- filter(df, grepl(filt, rules, fixed = TRUE))
        }
        rv$data <- df
        df
      }else{
        rv$data <- df
        df
      }
  })
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2]
    
    
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    d = data()
    # draw the histogram with the specified number of bins
    hst <- hist(d$lift,  col = 'lightblue', border = 'black')
  })
  
  output$collapsTree <- renderCollapsibleTree({
    d=data()
    selectedHier <<- input$bucketLHS
    print(selectedHier)
    
      if (length(selectedHier)<2){
        selectedHier <<-c("MODEL", "AGE", "FAULT_TYPE")
        # treeSummaryText <<- "Select two or more attributes."
      }
    
    collapsibleTreeSummary(
      d,  hierarchy = selectedHier, 
      width = 800, zoomable = FALSE,
      inputId = "treeUpdate"
    )
    
  })
  
  output$treeSummary <- renderPrint(str(input$treeUpdate))
  
  
  # Textfield - rule count
  output$ruleCount <- renderText({
    
    txt <- paste("Rules count:", nrow(rv$data))
    print(txt)
    txt}
  )
  
  # rules by target
  output$TargetBox <- renderPlot(
    {
      #data.model <<- read.csv(input$fileIn$datapath,
       #               sep = ",")
      print("Target Box start")
      df_cnts <- rv$data
      df_cnts <- as.data.frame( sort(table(df_cnts$FAULT_TYPE),decreasing =  TRUE))
      

      ggplot(df_cnts, aes(Var1,Freq),colo) + geom_col()   
    }
  )  
 
  
  output$decisionTree <- renderPlot({
    tree<- rpart(
      FAULT_TYPE ~ .,
      data = rv$data, 
      method = "class", 
      maxdepth = 6)
    prp(tree,
        box.palette = "BuGn",
        # box.palette = "auto",
        split.box.col = "lightgray",
        split.border.col = "darkgray",
    )
    
    
  }) 
 
  # output$Scatter <- renderPlotly(
  #   {
  #     # Get x and y axis, and find last part from supp conf lift in zaxis
  #     xaxis <- input$scatterX
  #     yaxis <- input$scatterY
  #     zaxis <- supconflif[supconflif != c(xaxis, yaxis)]
  #     
  #     print(paste("X: ", xaxis, " Y: ", yaxis, " Z: ", zaxis))
  #     
  #     d <-as.data.frame( rv$data)
  #     plot_ly(data = d , x = d[,xaxis], y = d[,yaxis], color = d[,zaxis])
  #     #~FAULT_TYPE)
  #   }
  # )
  

  # output$fileTable <- renderTable(
  #   {
  #    # req(input$fileIn)
  #     # when reading semicolon separated files,
  #     # having a comma separator causes `read.csv` to error
  #     tryCatch(
  #       {
  #         # df <<- read.csv(input$fileIn$datapath,
  #                        # sep = ",")
  #         # model.data <-reactive(df)
  #         df <- rv$data
  #         data()
  #        print(sum(df$support))
  #        rv$data
  #       },
  #       error = function(e) {
  #         # return a safeError if a parsing error occurs
  #         stop(safeError(e))
  #       }
  #     )
  #   }
  # ) # end fileTable
  
  output$fileTable2 <- DT::renderDT(
    options = list(scrollX = T),
    rv$data,
    filter = "top",
    )
  
# IDEA FROM https://stackoverflow.com/questions/38511717/how-do-i-get-the-data-from-the-selected-rows-of-a-filtered-datatable-dt  
  tableSelected <- reactive({
    ids <- input$fileTable2_rows_selected
    d <- rv$data
    d[ids,]
  })
  
  output$tableSelected <- DT::renderDataTable({
    DT::datatable(
      options = list(scrollX = T),
      tableSelected(),
      
      selection = list(mode = "multiple"),
      caption = "Selected Rows from  Data Table above"
    )
  })
  
  output$d3_test <- renderD3({
    print("D3 stuff")
    
    d = 
    r2d3(
      as_d3_data(df$lift),
      "barchart.js"
      
    )
  }
  )
  
  
} # end server