# Load Librarys
library(plotly)
library(ggplot2)
library(dplyr)
library(collapsibleTree)
library(rpart)
library(rpart.plot)
library(crosstalk) # double plot once with bscols

library(alluvial) # sankey plots


# Init global variables and helper functions

interesting_cols <-
  c(
    "FAULT_TYPE",
    "rules",
    "support",
    "confidence",
    "lift",
    "FAULT_COUNT",
    "MODEL",
    "AGE",
    "DEALERSHIP",
    "CUSTOMER_TYPE",
    "USER_CUSTOMISED",
    "COUNTRY",
    "GEO_TYPE",
    "OBJECT_COUNT",
    "FAULT_RATE",
    "AGGREGATION_LEVEL",
    "SET_SIZE"
  )

supconflif <- c("support", "confidence", "lift")

elementHeight <- 50 # standard elemt height for e.g. button

# care for categorical features
categoricalFeatures <-
  c(
    "FAULT_TYPE",
    "MODEL",
    "AGE",
    "DEALERSHIP",
    "CUSTOMER_TYPE",
    "USER_CUSTOMISED",
    "COUNTRY",
    "GEO_TYPE"
  )

xyzArr <- reactiveValues()

xyzArr$x = "support"
xyzArr$y = "confidence"
xyzArr$z = "KPI"


applyCategory <- function(data) {
  data[categoricalFeatures] <-
    lapply(data[categoricalFeatures], factor)
  
  data$SET_SIZE <- as.factor(data$SET_SIZE)
  data
}

df <- applyCategory(read.csv('SCHRITT_4_1_MINSUP_MINCONF.csv')[, interesting_cols]) # Initial dataset

df_names <- c('Init Dataset')
df_subsets <- list(list(df))


getSubset <- function(name) {
  tf <- df_names == name #where name is in subset
  df_subsets[tf] # retrun the corresponding subset
}


treeSummaryText <- ""

server <- function(input, output, session) {
  print("Start")
  rv <- reactiveValues()
  rv$data <- df
  compare_data <- reactiveValues()
  
  addSubset <- function (name, subset) {
    print(paste("Adding subset now", name))
    df_names <<- append(df_names, name)
    df_subsets <<- append(df_subsets, list(subset))
    
    updateSelectInput(session, "dropdown_subsets",
                      choices = df_names,
                      selected = name)
    updateSelectInput(session, "dropdown_plots1",
                      choices = df_names)
    updateSelectInput(session, "dropdown_plots2",
                      choices = df_names)
    
    
  }
  
  ###################
  # IPPUT SECTION   #
  ###################
  
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("newsubgroup", "Save subgroup",
                placeholder = 'Enter subgroup name'),
      
      
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
    df_subsets[2] %>%
      print()
    
    # session$sendCustomMessage(
    #   type = 'testmessage',
    #   message = 'Thank you for clicking')
  })
  
  
  observeEvent(input$btSaveSubset, {
    showModal(dataModal(failed = TRUE))
  })
  
  
  observeEvent(input$bt_add_subgroup, {
    # print('bt_add_subgroup OK Klick')
    
    #print(input$newsubgroup)
    addSubset(input$newsubgroup, rv$data)
    # updateSelectInput(session, "dropdown_subsets",
    #                   choices = df_names)
    removeModal()
  })
  
  observeEvent(input$dropdown_subsets, {
    print('dropdown activated')
    #print(getSubset(name = input$dropdown_subsets))
    rv$data <- as.data.frame(getSubset(name = input$dropdown_subsets))
    
  })

  # Button Upload
  data <-  observeEvent(input$fileIn, {
    df <<-
      read.csv(input$fileIn$datapath, sep = ",")[, interesting_cols]
    df <<- applyCategory(df)
    print("file loaded")
    addSubset(name =  strsplit(input$fileIn$name, ".", fixed = TRUE)[[1]][1],
              subset =  df)
    rv$data <- df
  })
  
  # Button Filter Table
  observeEvent(input$btFilterTable, {
    df <- rv$data[input$fileTable2_rows_all, ]
    rv$data <- df
    print("BT Filter clicked")
    #print(df)
  })

  
  # Filter data by txt input
  data <- eventReactive(input$txt_filter, {
    txt <- input$txt_filter
    
    if (txt != "") {
      df <- rv$data
      txtlist <- strsplit(txt, ';')
      
      for (i in 1:length(txtlist[[1]])) {
        filt <- txtlist[[1]][i]
        print(paste("Filter ", filt))
        df <- filter(df, grepl(filt, rules, fixed = TRUE))
      }
      rv$data <- df
      rv$data
    } else{
      print("data function")
      # rv$data <- getSubset(name = 'Init Dataset')
      # rv$data

      # df <- rv$data
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
  
  

  
  
  observeEvent(input$scatterX,{
    xyzArr$x = input$scatterX
  })
  
  observeEvent(input$scatterY,{
    xyzArr$y = input$scatterY
  })
  observeEvent(input$scatterColor,{
    xyzArr$c = input$scatterColor
  })
  
  
  
  
  getZAxis <- function(x, y) {
    # returns Z axis (support, conf, lift) - depends on x and y
    scl <- supconflif
    xy <-  c(x, y)
    
    if (x != y) {
      for (el in xy) {
        scl <- scl[scl != el]
      }
      return(scl)
    }
  }
  

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

  
  
  output$Scatter <- renderPlotly({
    # Get x and y axis, and find last part from supp conf lift in zaxis
    axs <- xyzArr
    #print(paste("X: ", xaxis, " Y: ", yaxis, " Z: ", zaxis))
    d <- rv$data#df#-as.data.frame( rv$data)
    
    # IF KPI -> take supp, conf or lift. Otherwhise selected feature
    if(input$scatterColor == "KPI"){
      axs$z <- getZAxis(axs$x, axs$y)
    }else{
      axs$z <- input$scatterColor
    }
    
    upd <- rv$data
    nms <- row.names(d)
    p <-
      plot_ly(
        data = d ,
        x = d[, axs$x],
        y = d[, axs$y],
        color = d[, axs$z],
       colors = 'Reds' ,
        type = "scatter",
        mode= "markers",
        key = nms,
        hoverinfo = 'text',
        text = ~ paste(
          'Fault: ',
          FAULT_TYPE,
          '\nSupp:',
          support ,
          '\nConf: ',
          confidence,
          '\nLift: ',
          lift
        )
      ) %>%
      highlight("plotly_selected", dynamic = TRUE)
    
    layout(
      p,
      xaxis = list(title = axs$x),
      yaxis = list(title = axs$y),
      dragmode = "lasso"
    )
    
    # fig <- fig %>% layout()
    #~FAULT_TYPE)
  })
  
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
  

  
  # output$sankeyPlot <- renderPlot({
  #   sankey_cols <- input$bucketLHS
  #   dats_all <- rv$data %>%
  #     group_by_at(vars(one_of(sankey_cols)))%>%  # group them# data
  #     # group_by( vars(sankey_cols)) %>%  # group them
  #     summarise(Freq = FAULT_COUNT)
  # 
  #   l <- length(sankey_cols)
  # 
  # 
  #   # now plot it
  #   alluvial( dats_all[,1:l],
  #             freq=dats_all$Freq,
  #             border= NA ,
  #             col =  "Red",
  #             alpha = 0.3
  #             )
  # 
  # })
  


  
  
  output$collapsTree <- renderCollapsibleTree({

    selectedHier <<- input$bucketLHS
    
      if (length(selectedHier)<2){
        selectedHier <<-c("MODEL", "AGE", "FAULT_TYPE")
      }
    
    d <- data()
    d <- rv$data
    d<- d[,c(selectedHier , "FAULT_COUNT")]
    
    form = as.formula(paste(". ~", paste(selectedHier, collapse = " + ")))
    d <- aggregate(form, data = d,FUN = sum)
    
    d$perc<-sapply(d$FAULT_COUNT, FUN = function(x)x/sum(d$FAULT_COUNT))
    
    # print(d)
    collapsibleTreeSummary(
      d,  hierarchy = selectedHier, 
      # percentOfParent = TRUE,
      root = "Cars",
      attribute = "FAULT_COUNT",
      # tooltip = "tooltip",
      #nodeSize = "leafCount",
      #width = 800, 
      zoomable = FALSE,
      inputId = "treeUpdate"
    )
    
  })
  
  
  asRelativeValues <- function(li){
    # returns the relative values of a lists
    s <- sum(li)
    rel<-sapply(li, FUN = function(x) x/s)
    rel
  }
 
  
  # Observe Tree and handle several plots on page and train tree for suggestion
  # Updated Uputpus: Information, suggestion plot with Tree and sankey chart
  observeEvent(
    {input$treeUpdate 
      input$treeDepth}, {
    
    treUpd <-input$treeUpdate
    
    # INFO ABOUT SELECTION IN TREE
    output$treeSummary <- renderPrint(
      if (is.null(treUpd)){
        str("Show tree selection")
      }else{
        str(treUpd)
      }
      
      )
  
    d <-rv$data
    
    # Prepare Data
    for (el in names(treUpd)) {
      filter_col <- el
      filter_val <- 
        print(paste("For col", el, "filter by", treUpd[el]))
      
      d<-d[d[,el]==treUpd[el],]
    }
    
    # Train tree with filtered Data and plot 
    d_sub <- subset(d, select= -c(rules,support,confidence,
                                  lift,FAULT_COUNT, OBJECT_COUNT
                                  #FAULT_RATE#
                                  ,AGGREGATION_LEVEL,SET_SIZE
    ))
    tree_r<- rpart(
      FAULT_RATE ~ .,
      #FAULT_TYPE ~ .,
      data = d_sub,
      method = "anova",# "class",
      maxdepth = input$treeDepth)
    

    output$supportTree <- renderPlot({
      rpart.plot(tree_r , box.palette = "Reds", snip = TRUE, )
      # prp(tree_r,
      #     box.palette = "BuGn",
      #     # box.palette = "auto",
      #     split.box.col = "lightgray",
      #     split.border.col = "darkgray",
      #     branch.type = 5,
      #     # uniform=TRUE
      # , snip = TRUE
      # )
    })
    
  # INFOTEXT NEXT BEST FEATURE
    suggestionText <- paste("Next best feature by regression Tree: \n", rpart.rules(tree_r)[[3]][1],"\nConsider feature values:", rpart.rules(tree_r)[[5]][1])
    output$treeSuggestion <- renderText(suggestionText)
    
    
    output$sankeyPlot <- renderPlot({
      sankey_cols <- input$bucketLHS
      # sankey_cols<-selectedHier
      dats_all <- df %>%      
        group_by_at(vars(one_of(sankey_cols)))%>%  # group them# 
        # group_by( vars(sankey_cols)) %>%  # group them
        summarise(Freq = FAULT_COUNT) 
      
      # SANKEY PLOT
      l <- length(sankey_cols)
      
      # color managing
      color_feature <- selectedHier[1] # which featur should be colored
      color_classes <- names(table(dats_all[,color_feature])) # which unique values has a feature
      ncolors <- length(color_classes) # how many fetures 
      color_plate <- brewer.pal(n = ncolors, name = "Set1") # create color palete with n colors

      dats_all$colorClass <- sapply(dats_all[color_feature] ,FUN= function(x) color_plate[match(x, color_classes)])

      
      # PLOT SANKEY
      alluvial( dats_all[,1:l],
                freq=dats_all$Freq,
                border= NA ,
                col = dats_all$colorClass[,1],#color_plate[match(dats_all[color_feature], color_classes)],sep=""),#ifelse( dats_all$AGE == "*", "Red", "purple"),
                alpha = 0.7
                
      ) #+ geom_alluvium(aes(fill = FAULT_TYPE), width = 1/12)
      
    })
    
    
    
  
    
  })
  
  
  
  
  
  # Textfield - rule count
  output$ruleCount <- renderText({
    
    txt <- paste("Rules count:", nrow(rv$data))
    print(txt)
    txt}
  )
  
# 
#  output$treeSuggestion <- renderText(
#    {
#      "Here my suggestion!"
#    }
#  )
  
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
            box.palette = "Reds",
            extra=101, #number and percentage of objects
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
            box.palette = "Reds",
            extra = 8, # prop of fitted class --> accuracy
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
  
  
  
  
  #### PLOTS 
  
  
  
  
 
  getPlot <- function(feature ){
    # Create a barplot by given feature
    
    # d1<-getSubset(name = input$dropdown_plots1)
    # d2<-getSubset(name = input$dropdown_plots2)
    # 
  
    print("Plot Feature:")
    print(compare_data$d1)
    d1<-compare_data$d1
    d2<-compare_data$d2
    
    
    # t$Freq <- as.data.frame( table(d1[feature]))
    # t$Freq2 <- table(d2[feature])$Freq
    t <- as.data.frame(table(d1[feature]))
    t$Freq2 <- as.data.frame(table(d2[feature]))$Freq
    
    # t$Freq <- as.data.frame(asRelativeValues( table(d1[feature])))[,1]
    # t$Freq2 <- asRelativeValues( as.data.frame(table(d2[feature]))$Freq)
    # 
    if (input$dropdown_absRel =='relative'){
      
      t$Freq <- asRelativeValues(t$Freq)
      t$Freq2 <- asRelativeValues(t$Freq2)
      
    }
    
    fig <-
      plot_ly(
        t,
        x = ~ Var1,
        y = ~ Freq,
        type = 'bar',
        name = input$dropdown_plots1
      )
    fig <- fig %>% add_trace(y = ~ Freq2, 
                             name = input$dropdown_plots2
                             )
    fig <- fig %>% layout(xaxis = list(title = feature))
    fig
    
  }
    
 createPlots <- function(){
   print(paste("make plots", input$dropdown_plots1))
   print(as.data.frame(getSubset(name =  input$dropdown_plots1)))
   
   
   if(input$dropdown_plots1 != input$dropdown_plots2){
     compare_data$d1 <- as.data.frame(getSubset(name =  input$dropdown_plots1))
     compare_data$d2 <- as.data.frame(getSubset(name =  input$dropdown_plots2))
    
     
      # for (i in 1:8) {
      #   plt_name <- paste("plots_",i, sep = "") # to know which plot to use
      #   print(i)
      #   output[[plt_name]] <- renderPlotly({getPlot(categoricalFeatures[i])})
      # }
     output[["plots_info"]] <- renderText({""})
     output[["plots_1"]] <- renderPlotly({getPlot(categoricalFeatures[1])})
     output[["plots_2"]] <- renderPlotly({getPlot(categoricalFeatures[2])})
     output[["plots_3"]] <- renderPlotly({getPlot(categoricalFeatures[3])})
     output[["plots_4"]] <- renderPlotly({getPlot(categoricalFeatures[4])})
     output[["plots_5"]] <- renderPlotly({getPlot(categoricalFeatures[5])})
     output[["plots_6"]] <- renderPlotly({getPlot(categoricalFeatures[6])})
     output[["plots_7"]] <- renderPlotly({getPlot(categoricalFeatures[8])})
     output[["plots_8"]] <- renderPlotly({getPlot(categoricalFeatures[7])})
   }else{
     output[["plots_info"]] <- renderText({"Please select a different subset to compare."})
     
   }
   
   
  
  }
  
  observeEvent(input$dropdown_plots1, {
    print('dropdown plots 1')
    print(input$dropdown_plots1)
    
    createPlots()
   # print(getSubset(name = input$dropdown_subsets))
  #  rv$data <- as.data.frame(getSubset(name = input$dropdown_subsets))
    
  })
  
  observeEvent(input$dropdown_plots2, {
    print('dropdown plots 2')
    print(input$dropdown_plots2)
    
    createPlots()
    # print(getSubset(name = input$dropdown_subsets))
    #  rv$data <- as.data.frame(getSubset(name = input$dropdown_subsets))
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
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