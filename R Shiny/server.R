# Load Librarys
library(plotly)
library(ggplot2)
library(dplyr)
library(collapsibleTree)
library(rpart)
library(rpart.plot)
library(crosstalk) # double plot once with bscols

library(alluvial) # sankey plot
library(RColorBrewer)
library(arules)
library(arulesViz)

library(visNetwork)

# Init global variables and helper functions

options(shiny.maxRequestSize=100*1024^2) # increase file upload limit

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

elementHeight <- 50 # standard element height for e.g. button

# consider these categorical features
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
treUpd <- ""
color_light <- '#FFFAFA'

xyzArr <- reactiveValues()

xyzArr$x = "support"
xyzArr$y = "confidence"
xyzArr$z = "KPI"

applyCategory <- function(data) {
  # featuresin DF as factor
  data[categoricalFeatures] <-
    lapply(data[categoricalFeatures], factor)
  
  data$SET_SIZE <- as.factor(data$SET_SIZE)
  data
}

df <- applyCategory(
  read.csv('SCHRITT_4_1_MINSUP_MINCONF.csv')[, interesting_cols]) # Initial dataset

df_names <- c('Init Dataset')
df_subsets <- list(list(df))


treeSummaryText <- ""

server <- function(input, output, session) {
  print("Start")
  rv <- reactiveValues()
  rv$data <- df
  compare_data <- reactiveValues()
  selectedHier <- reactiveValues()
  
  getSubset <- function(name) {
    # return a subset
    if (name == 'Current Data'){
      print(c('current data required\n', rv$data))
      
      rv$data
    }else{
      tf <- df_names == name #where name is in subset
      df_subsets[tf] # retrun the corresponding subset
    }
  }
  
  addSubset <- function (name, subset) {
    print(paste("Adding subset", name))
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
  # INPUT SECTION   #
  ###################
  
  dataModal <- function(failed = FALSE) {
    # Dialogbox to save a subset 
    modalDialog(
      textInput("newsubgroup", "Save subgroup",
                placeholder = 'Enter subgroup name'),
      
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("bt_add_subgroup", "OK")
      ),
    )
  }
  
  observeEvent(input$do, {
    # depricated
    rv$data <- df
    df_subsets[2] %>%
      print()
  })
  
  
  observeEvent(input$btSaveSubset, {
    showModal(dataModal(failed = TRUE))
  })
  
  observeEvent(input$btUseTreeSelection, {
    
    d <- rv$data
    d <- filterDfByTreeselectionfunction(d)
    rv$data <-d
    
  })
  
  observeEvent(input$bt_add_subgroup, {

    addSubset(input$newsubgroup, rv$data)
    removeModal()
  })
  
  observeEvent(input$dropdown_subsets, {
    print('dropdown activated')
    rv$data <- as.data.frame(getSubset(name = input$dropdown_subsets))
    
  })

  # UPLOAD FILE WITH RULES
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
      rv$data <- df
      df
    }
  })
  
  observeEvent(input$click_bar,{
    # not realized 
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
    print(input$scatterColor)
    xyzArr$z = input$scatterColor
  })
  
  
  filterDfByTreeselectionfunction <- function(d){
    # Prepare Data - 
    print(c("treupd val",str(treUpd)))
    if (!is.null(treUpd)){
      for (el in names(treUpd)) {
        filter_col <- el
        filter_val <- 
          print(paste("For col", el, "filter by", treUpd[el]))
        
        d<-d[d[,el]==treUpd[el],]
        summary(d)
      } 
        return(d)
      
    }
  }
  
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

    d <- rv$data
    
    # IF KPI -> take supp, conf or lift. Otherwhise selected feature
    if(input$scatterColor == "KPI"){
      axs$z <- getZAxis(axs$x, axs$y)
      col_scale <- "Reds"
    }else{
      axs$z <- input$scatterColor
      col_scale <-colorRampPalette( brewer.pal(9,"YlOrRd") )(8+length(unique(d[, axs$z])))# brewer.pal(n = length(unique(d[, axs$z])), name = "YlOrRd")
      #cut off some xellow 
      col_scale <- col_scale[-c(1:4)]
      
    }
    
    upd <- rv$data
    nms <- row.names(d)
    p <-
      plot_ly(
        data = d ,
        x = d[, axs$x],
        y = d[, axs$y],
        color = d[, axs$z],
       colors = col_scale,# 'Reds' ,
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
      # layout(legend = list(orientation = 'v'))%>%
      layout(plot_bgcolor= "#fffbfb",
    paper_bgcolor = color_light) %>%
      highlight("plotly_selected", dynamic = TRUE)
    
    layout(
      p,
      title= paste("Rules by", axs$z),
      xaxis = list(title = axs$x),
      yaxis = list(title = axs$y),
      dragmode = "lasso"
    )
  })
  
  # rules by target bar plot
  output$TargetBox <- renderPlot(
    height = 200,
    {
      df_cnts <- rv$data
      df_cnts <- as.data.frame( sort(table(df_cnts$FAULT_TYPE),decreasing =  TRUE))
      ggplot(df_cnts, aes(Var1,Freq)  )  + geom_col(fill='Red', alpha =0.75)   
    }
  )  
  
  
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
 
  
  # Observe Tree-Plot and handle several plots on page and train tree for suggestion
  # Updated Outputs: Information, suggestion plot with Tree and sankey chart
  
  observeEvent(
    {input$treeUpdate 
      input$treeDepth}, {
    
    treUpd <<-input$treeUpdate
    
    
    d <-rv$data
    # Prepare Data
    for (el in names(treUpd)) {
      filter_col <- el
      filter_val <-
        print(paste("For col", el, "filter by", treUpd[el]))

      d<-d[d[,el]==treUpd[el],]
    }
    
    # Prepare data, filtered Data and plot 
    df_sub <- subset(d, select= -c(rules,support,confidence,
                                  lift,#FAULT_COUNT, OBJECT_COUNT
                                  #FAULT_RATE#
                                  # AGGREGATION_LEVEL,
                                  SET_SIZE
                                  ))
    
    faults_by_treepath <<-sum(df_sub$FAULT_COUNT)
    objs_by_treepath <<- sum(df_sub$OBJECT_COUNT)
    
    # INFO ABOUT SELECTION IN TREE
    output$treeSummary <- renderPrint(if (!is.null(treUpd)) {
      cat(
        str(treUpd), "\n",
        "Faults:",
        faults_by_treepath,
        "\n Objects:",
        objs_by_treepath,"\n F.Rate:",
        1000*faults_by_treepath/objs_by_treepath
      )
      
    })
    
    
    ########################################################

    # CREATE a HEATMAP - representing two features and the weighted faultrate
    
    # find features for heatmap
        
    heat_feat <- selectedHier[!selectedHier %in% names(treUpd)] # drop selected features from collapsed tree from the hierachy 
  
    #PLOT HEATMAP ONLY IF TRERE ARE 2 features
        if (length(heat_feat)<2){
          heat_feat <- selectedHier[1:3]
        }
       
          # create a formula to aggregate. use faultcount and objectcount to compute the rel. faultrate
          form = as.formula(paste("cbind(FAULT_COUNT, OBJECT_COUNT) ~", paste(c(heat_feat, "AGGREGATION_LEVEL"), collapse = " + ")))
          
          t <- aggregate(form, data = df_sub,FUN = sum) # create aggregation table
          t$f_rate <- t$FAULT_COUNT*100/t$OBJECT_COUNT # compute rel. faultrate
          
          # PLOT NUMBER OF AGGREGATION LEVELS
          x <-t$AGGREGATION_LEVEL %>%
            table() %>%
            as.data.frame()
          
          f <- plot_ly (x= x$., y=x$Freq, type = 'bar' , marker = list(color = '#ff7560')) %>%
            layout(plot_bgcolor= color_light,
                   paper_bgcolor = color_light, title= "Aggregation levels", 
                   xaxis=list(title="Aggregation levels", showgrid = F),
                   yaxis=list(title="# of rules", showgrid = F))
          
          # filter aggregated data. Use the desired features which are agg level 5 
          theat <- subset(t,  select = c(heat_feat, "f_rate"), AGGREGATION_LEVEL == length(names(treUpd)),)
          
          # CREATE HEATMAP. Features vlues are X and Y axis, the value is the new fault rate
          heatmp <- plot_ly(x= theat[,1], y=theat[,2], z = theat$f_rate,
                            type = "heatmap", colors = 'Reds' ) %>%
            layout(plot_bgcolor= color_light,#'#c0b0b0',#ffffff',
                   paper_bgcolor = color_light,# '#FFeaea',
                   title= "Aggregated relative fault rate",
                   xaxis=list(title = "", showgrid = F),
                   yaxis=list(title="", showgrid = F))# %>%
          
          output$aggregatedFaultsHeat <- renderPlotly({heatmp})
      
      
        # }
    # Drop columns which are not useful for the treex
    df_sub <- select(df_sub, -c(FAULT_COUNT, OBJECT_COUNT, AGGREGATION_LEVEL))
    
    # TRAIN AND PLOT REGRESSIONTREE 
    tree_r<- rpart(
      FAULT_RATE ~ .,
      data = df_sub,
      method = "anova",# "class",
      maxdepth = input$treeDepth)
    
    
      output$supportTree <- renderPlot({
      rpart.plot(tree_r , box.palette = "Reds", snip = TRUE, )
    })
    
    # INFOTEXT NEXT BEST FEATURE
    
    featSuggestion <- rpart.rules(tree_r)[[3]][1] # Feature by tree
    possibleFeatures <- as.vector(unique(df_sub[[featSuggestion]]))
    print(c("Possible features: ", possibleFeatures))
    featSuggestionTree <-rpart.rules(tree_r)[[5]][1] 
    
    featSuggestionTree <-  strsplit(featSuggestionTree,' or ')[[1]]
    print("Split VAUES: ")
    print(featSuggestionTree)
    # feature values where to split
    
    feat_diff <- possibleFeatures[!possibleFeatures %in% featSuggestionTree]
    print(c("feat diff",feat_diff))
    suggestionText <- paste("Next best feature by regression Tree: \n", featSuggestion,"\nConsider feature values:\n")
    
    for (f in feat_diff){
      suggestionText <- paste(suggestionText, f)
    }
    
    output$treeSuggestion <- renderText(suggestionText)
    
    #########################################################
    
    # PLOT SANKEY
    output$sankeyPlot <- renderPlot({
      sankey_cols <- input$bucketLHS
      # sankey_cols<-selectedHier
      dats_all <- d %>%      
        group_by_at(vars(one_of(sankey_cols)))%>%  # group them ..
        summarise(Freq = FAULT_COUNT) # .. and summarise
      
      # SANKEY PLOT
      l <- length(sankey_cols)
      
      # color managing - for some reason working proper in R Studio but not in webbrowser
      color_feature <- selectedHier[1] # which feature should be colored
      color_classes <- names(table(dats_all[,color_feature])) # which unique values has a feature
      ncolors <- length(color_classes) # how many fetures 
      color_plate <- c('blue', 'red')
      
      if (ncolors >2){
        color_plate <- brewer.pal(n = ncolors, name = "YlOrRd") # create color palete with n colors
      }

      dats_all$colorClass <- sapply(dats_all[color_feature] ,FUN= function(x) color_plate[match(x, color_classes)])
      
      #Replace * Values with color white
      fkn <- function(a,b){
        ifelse(a == "*", "white", b)
      }
      
      if(input$dropdownHideStar =='yes'){
        dats_all$colorClass <- mapply(fkn, dats_all[color_feature], dats_all['colorClass'])
      }
      

    
      # PLOT SANKEY
      alluvial( dats_all[,1:l],
                freq=dats_all$Freq,
                border= NA ,
                col = dats_all$colorClass[,1],
                alpha = 0.7

      ) 
    })
    
  })
  
  

  # Textfield - rule count
  output$ruleCount <- renderText({
    
    txt <- paste("Rules count:", nrow(rv$data))
    print(txt)
    txt}
  )
  
  output$decisionTreeRegression <- renderPlot({
    
    df_sub <-rv$data[, interesting_cols]
    df_sub <- subset(df_sub, select= -c(rules,support,confidence,
                                    lift,FAULT_COUNT, OBJECT_COUNT
                                    ,AGGREGATION_LEVEL,SET_SIZE
    ))
    
    tree_r<- rpart(
      FAULT_RATE ~ .,
      data = df_sub,
      method = "anova",
      maxdepth = 3)
    

    prp(tree_r,
            box.palette = "Reds",
            extra=101, #number and percentage of objects
            compress = TRUE,
            split.box.col = "lightgray",
            split.border.col = "darkgray",
    )
    
    
  }) 
  output$decisionTreeClass <- renderPlot({

    df_sub <- rv$data[, interesting_cols]
    df_sub <- subset(df_sub, select= -c(rules,support,confidence,
                                         lift,FAULT_COUNT, OBJECT_COUNT
                                         ,FAULT_RATE
                                         ,AGGREGATION_LEVEL,SET_SIZE
    ))

    
    tree_c<- rpart(
      FAULT_TYPE ~ .,
      data = df_sub,
      method = "class",
      maxdepth = 3)
    
    prp(tree_c,
            box.palette = "Reds",
            extra = 8, # prop of fitted class --> accuracy
            split.box.col = "lightgray",
            split.border.col = "darkgray",
    )
    
  }) 
  
  output$fileTable2 <- DT::renderDT(
    options = list(scrollX = T),
    rv$data,
    filter = "top",
    )

  #### PLOTS 
 
  getPlot <- function(feature, showLegend ){
    # Create a barplot by given feature

    d1<-compare_data$d1
    d2<-compare_data$d2
    

    t1 <- as.data.frame(table(d1[feature]))
    t2 <- as.data.frame(table(d2[feature]))
    tj <- merge(t1, t2, by = "Var1", all = T)
    tj[is.na(tj)] <- 0
    
    if (input$dropdown_absRel =='relative'){
      
      tj$Freq.x <- asRelativeValues(tj$Freq.x)
      tj$Freq.y <- asRelativeValues(tj$Freq.y)
      
    }
    
    fig <-
      plot_ly(
        tj,
        x = ~ Var1,
        y = ~ Freq.x,
        type = 'bar',
        name = input$dropdown_plots1, 
        marker = list(color = "#b33625")
      ) %>%
      layout(plot_bgcolor = color_light,
             paper_bgcolor = color_light)
    
    fig <-
      fig %>% layout(xaxis = list(title = feature, showgrid = F),
                     yaxis = list(showgrid = F,
                                  title = paste(input$dropdown_absRel, " Freq")
                                  ),
                     showlegend = showLegend
                     )
    
    if (input$dropdown_plots1 != input$dropdown_plots2) {
      fig <- fig %>% add_trace(
        y = ~ Freq.y,
        name = input$dropdown_plots2,
        marker = list(color = '#d95a00')
      )
    }
    
    fig
    
  }
  
  toTransactions <- function(df_trans){
    
    cars_data_full <- df_trans[rep(seq_len(nrow(df_trans)), df_trans$FAULT_COUNT),]
    if (nrow(cars_data_full)>0){
    cars_data_full <- subset(cars_data_full, select=-c(AGGREGATION_LEVEL, 
                                                         FAULT_RATE,FAULT_COUNT,
                                                       OBJECT_COUNT,support,confidence, lift, SET_SIZE
                                                       ))
      cars_data_full_t <- as(cars_data_full, "transactions")
      rules <- apriori(cars_data_full_t, parameter = list(supp = 0.2, conf = 0.3))
      
      rules<-subset(rules, subset=rhs %pin% "FAULT_TYPE=" )

      output$rules_graph <- renderVisNetwork({plot(rules, method = "graph"
                                                , engine = "visNetwork"
                                              )})
      
    }
    
  }
    
 createPlots <- function(){
   # create plots for the 'Plots' ribbon
   
     compare_data$d1 <- as.data.frame(getSubset(name =  input$dropdown_plots1))
     compare_data$d2 <- as.data.frame(getSubset(name =  input$dropdown_plots2))
    
     toTransactions(compare_data$d1)

     output[["plots_info"]] <- renderText({""})
     output[["plots_1"]] <- renderPlotly({getPlot(categoricalFeatures[1], TRUE)})
     output[["plots_9"]] <- renderPlotly({getPlot("AGGREGATION_LEVEL", FALSE)})
     output[["plots_2"]] <- renderPlotly({getPlot(categoricalFeatures[2], FALSE)})
     output[["plots_3"]] <- renderPlotly({getPlot(categoricalFeatures[3], FALSE)})
     output[["plots_4"]] <- renderPlotly({getPlot(categoricalFeatures[4], FALSE)})
     output[["plots_5"]] <- renderPlotly({getPlot(categoricalFeatures[5], FALSE)})
     output[["plots_6"]] <- renderPlotly({getPlot(categoricalFeatures[6], FALSE)})
     output[["plots_7"]] <- renderPlotly({getPlot(categoricalFeatures[8], FALSE)})
     output[["plots_8"]] <- renderPlotly({getPlot(categoricalFeatures[7], FALSE)})

  }
  
  observeEvent(input$dropdown_plots1, {
    print('dropdown plots 1')
    print(input$dropdown_plots1)
    
    createPlots()
    
  })
  
  observeEvent(input$dropdown_plots2, {
    print('dropdown plots 2')
    print(input$dropdown_plots2)
    
    createPlots()
    
  })
  
} # end server