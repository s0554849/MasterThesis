# Load Librarys

library(plotly)
library(ggplot2)
library(dplyr)
library(collapsibleTree)
library(rpart)
library(rpart.plot)
library(crosstalk) # double plot once with bscols

# Init global variables and helper functions

interesting_cols <- c("FAULT_TYPE","rules","support","confidence","lift",
                      "FAULT_COUNT","MODEL","AGE","DEALERSHIP","CUSTOMER_TYPE",
                      "USER_CUSTOMISED", "COUNTRY","GEO_TYPE", "OBJECT_COUNT", 
                      "FAULT_RATE",  "AGGREGATION_LEVEL", "SET_SIZE" )

supconflif <- c('support', 'confidence','lift')

elementHeight <- 50 # standard elemt height for e.g. button

# care for categorical features
categoricalFeatures <- c("FAULT_TYPE","MODEL","AGE","DEALERSHIP","CUSTOMER_TYPE",
               "USER_CUSTOMISED", "COUNTRY","GEO_TYPE")

applyCategory <- function(data){
  data[categoricalFeatures] <- lapply(data[categoricalFeatures], factor)
  data
}
df <- applyCategory(read.csv('SCHRITT_4_1_MINSUP_MINCONF.csv')[,interesting_cols]) # Initial dataset 

df_names <- c('Init Dataset')
df_subsets <- list(list(df))

addSubset <- function (name, subset){
  
  df_names<<-append(df_names, name)
  df_subsets <<- append(df_subsets, list(subset))
}

getSubset <- function(name){
  tf <- df_names ==name #where name is in subset
  df_subsets[tf] # retrun the corresponding subset
}


treeSummaryText <- ""

server <- function(input, output, session) {
  print("Start")
  rv <-reactiveValues()
  

  
# IPPUT SECTION
  
  
  # Observer Function- to update all views
    dataModal <- function(failed = FALSE) {
      
      modalDialog(
        textInput("newsubgroup", "Save subgroup",
                  placeholder = 'Enter subgroup name'
        ),
        
      
        footer = tagList(
          modalButton("Cancel"),
          actionButton("bt_add_subgroup", "OK")
        ),
        # addSubset('test', df),
        # print(new_df),
      )
    }
  observeEvent(input$do, {
    rv$data <- df
    df_subsets[2]%>%
      print()
    
    # session$sendCustomMessage(
    #   type = 'testmessage',
    #   message = 'Thank you for clicking')
  })
  
  observeEvent(input$btSaveSubset, {
    showModal(dataModal(failed = TRUE))
    }
  )
  
  
  observeEvent(input$bt_add_subgroup, {
    print('bt_add_subgroup OK Klick')
    
    print(input$newsubgroup)
    addSubset(input$newsubgroup, rv$data)
      updateSelectInput(session, "dropdown_subsets",
      #label = paste("Select input label", length(df_names)),
          choices = df_names
      #selected = tail(df_names, 1)
      )
      
      # span('(Try the name of a valid data object like "mtcars", ',
      # 'then a name of a non-existent object like "abc")'),
      # if (failed)
      # div(tags$b("Invalid name of data object", style = "color: red;")),
      
      
      removeModal()
    # } else {
    #   showModal(dataModal(failed = TRUE))
    # }
  })
  
  observeEvent(input$dropdown_subsets,{
    print('dropdown activated')
    print(getSubset(name =input$dropdown_subsets ))
    rv$data <- as.data.frame(getSubset(name =input$dropdown_subsets ))
    #df <- rv$data
    
  })

# Button Upload
  data <-  observeEvent(input$btUpload, {
      print("BT clicked")
      df <<- read.csv(input$fileIn$datapath,sep = ",")[,interesting_cols]
      df<<-applyCategory(df)
      
      rv$data <- df
    })

# Button Filter Table 
    observeEvent(input$btFilterTable, {
    
    df<-df[input$fileTable2_rows_all,]
    rv$data <- df
    print("BT Filter clicked")
    #print(df)
  })

  
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



  observeEvent(input$click_bar,{
    #df_cnts<-rv$data
    #print(nearPoints(df_cnts, input$plot1_click, addDist = TRUE))
  })
  
  observeEvent(input$plot1_brush,{
    print(input$plot1_brush)
    #brushedPoints(df_cnts, input$plot1_brush)
    #print("brush on barchart")
  })
  
  
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
  
  
  
  # Scattter Plot
  
  nms <- row.names(df)
  
  observeEvent(event_data("plotly_selected"),{
    d <- event_data("plotly_selected")
    df2 <- as.data.frame( df)
    if (!is.null(d)) {
      rv$data <-df2 %>%
        filter(row.names(df2) %in% d$key)
    }
  
  })

  
  
  output$Scatter <- renderPlotly(
    {
      # Get x and y axis, and find last part from supp conf lift in zaxis
      axs <- xyzArr
      #print(paste("X: ", xaxis, " Y: ", yaxis, " Z: ", zaxis))
      z <- getZAxis(axs$x,axs$y)
      upd <- rv$data
      d <-df#-as.data.frame( rv$data)
      nms <- row.names(d)
      p <- plot_ly(data = d , x = d[,axs$x], y = d[,axs$y], color = d[,axs$z],colors ='Reds' , type= "scatter", 
                   key = nms,log="x",
                   hoverinfo = 'text',
                   text = ~paste('Fault: ', FAULT_TYPE,'\nSupp:', support , '\nConf: ', confidence, '\nLift: ', lift)
                   )%>% 
        add_markers(alpha = 1) %>%
        highlight("plotly_selected", dynamic = TRUE)
        
      layout( p,
              xaxis = list(title= axs$x),
              yaxis = list(title= axs$y),
              dragmode = "lasso"
              )
      
      # fig <- fig %>% layout()
      #~FAULT_TYPE)
    }
  )
  
  # rules by target bar plot
  output$TargetBox <- renderPlot(
    height = 200,
    {
      #data.model <<- read.csv(input$fileIn$datapath,
      #               sep = ",")
      print("Target Box start")
      df_cnts <- rv$data
      df_cnts <- as.data.frame( sort(table(df_cnts$FAULT_TYPE),decreasing =  TRUE))
      ggplot(df_cnts, aes(Var1,Freq)  )  + geom_col(fill='Red', alpha =0.75)   
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
  

  
  
  output$collapsTree <- renderCollapsibleTree({

    selectedHier <<- input$bucketLHS
    print(selectedHier)
    
      if (length(selectedHier)<2){
        selectedHier <<-c("MODEL", "AGE", "FAULT_TYPE")
        # treeSummaryText <<- "Select two or more attributes."
      }
    d= data()
    
    collapsibleTreeSummary(
      d,  hierarchy = selectedHier, 
      root = "Cars",
      attribute = "OBJECT_COUNT",
      #nodeSize = "leafCount",
      width = 800, zoomable = TRUE,
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
  

 
  
  output$decisionTreeRegression <- renderPlot({
    #selectedHier <<- input$bucketLHS
    #äropcols <- append(selectedHier)
    
    df_sub <- subset(rv$data, select= -c(rules,support,confidence,
                                    lift,FAULT_COUNT, OBJECT_COUNT
                                    #FAULT_RATE#
                                    ,AGGREGATION_LEVEL,SET_SIZE
    ))
    tree_r<- rpart(
      FAULT_RATE ~ .,
      #FAULT_TYPE ~ .,
      data = df_sub,
      method = "anova",# "class",
      maxdepth = 3)
    
   

    prp(tree_r,
            box.palette = "BuGn",
            # box.palette = "auto",
            split.box.col = "lightgray",
            split.border.col = "darkgray",
    )
    
    
  }) 
  output$decisionTreeClass <- renderPlot({
    #selectedHier <<- input$bucketLHS
    #äropcols <- append(selectedHier)
    
    df_sub <- subset(rv$data, select= -c(rules,support,confidence,
                                         lift,FAULT_COUNT, OBJECT_COUNT
                                         #FAULT_RATE#
                                         ,AGGREGATION_LEVEL,SET_SIZE
    ))

    
    tree_c<- rpart(
      #FAULT_RATE ~ .,
      FAULT_TYPE ~ .,
      data = df_sub,
      method = "class",
      maxdepth = 3)
    
    prp(tree_c,
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
  # # tableSelected <- reactive({
  # #   ids <- input$fileTable2_rows_selected
  # #   d <- rv$data
  # #   d[ids,]
  # # })
  # 
  # # output$tableSelected <- DT::renderDataTable({
  # #   DT::datatable(
  # #     options = list(scrollX = T),
  # #     tableSelected(),
  # #     
  # #     selection = list(mode = "multiple"),
  # #     caption = "Selected Rows from  Data Table above"
  # #   )
  # # })
  
  # output$d3_test <- renderD3({
  #   print("D3 stuff")
  #   
  #   
  #   exclusion(df) <- NULL
  #   d = 
  #   r2d3(
  #     as_d3_data(df$lift),
  #     "barchart.js"
  #     
  #   )
  # }
  # )
  
  
} # end server