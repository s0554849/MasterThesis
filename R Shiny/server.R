# TODO

# Button und 
# Filter

library(plotly)
library(dplyr)
df <- read.csv("/Users/benjaminwuthe/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Joshuas stuff/Daten/SCHRITT_4_1_MINSUP_MINCONF.csv")

server <- function(input, output, session) {
  print("Start")
  
  data()

  
  # Observer Function- to update all views
  observeEvent(input$do, {
    session$sendCustomMessage(
      type = 'testmessage',
      message = 'Thank you for clicking')
  })

data<-  observeEvent(input$btUpload, {
    print("BT clicked")
    df <<- read.csv(input$fileIn$datapath,sep = ",")
  })
  
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
        df
      }else{
        print("No Txt-Filter")
        df
      }
  })
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2]
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    d = data()
    # draw the histogram with the specified number of bins
    hst <- hist(d$lift,  col = 'darkgray', border = 'white')
    
  })
  

  # Textfield - rule count
  output$ruleCount <- renderText({
    
    txt <- paste("Anzahl Regeln:", nrow(data()))
    print(txt)
    txt}
  )
  
  # rules by target
  output$TargetBox <- renderPlotly(
    {
      #data.model <<- read.csv(input$fileIn$datapath,
       #               sep = ",")
      print("Target Box start")
      df_cnts <- data()
      df_cnts <- as.data.frame( sort(table(df_cnts$FAULT_TYPE),decreasing =  FALSE))
      
      TargetBox <- plot_ly(
        y=df_cnts$Var1,
        x=df_cnts$Freq,
        
        type = "bar",
        orientation = 'h'
      )
    }
  )  

  output$fileTable <- renderTable(
    {
     # req(input$fileIn)
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          # df <<- read.csv(input$fileIn$datapath,
                         # sep = ",")
          # model.data <-reactive(df)
         print(sum(df$support))
      
          data()
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
    }
  ) # end fileTable
} # end server