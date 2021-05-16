library(shinythemes)
library(bslib)
library(shinyWidgets)
library(plotly)
library(sortable)
library(r2d3)
library(collapsibleTree)
library(visNetwork)

items <-
  c(
    "AGE",
    "COUNTRY",
    "MODEL",
    "DEALERSHIP"	,
    "CUSTOMER_TYPE",
    "USER_CUSTOMISED",
    "GEO_TYPE",
    "FAULT_TYPE"
  )
supconflif <- c('support', 'confidence', 'lift')

style_light <- "background-color: #FFFAFA;"
style_dark <-"#faf1f1"

initPlotsChoice <- c('Initial Data')





ui <- fluidPage(
  
  # theme = bs_theme(version = 4, bootswatch = "sketchy", bg = "Red",fg = "Black" ),
  theme = shinytheme("united"),
  # theme = "simplex.min.css",
  
  setBackgroundColor(color =style_dark),
  windowTitle = "Subgroup discovery",
  
  #icon("cog", lib = "glyphicon"),
  # Application title
  
  
  tags$head(tags$script(src = "message-handler.js")),
  # ff6644
  # Sidebar for user inputs
  navbarPage("Subgroup explorer",
             
             tabPanel(
               "Plot",
               
               sidebarLayout(
                 sidebarPanel(
                   style = style_light,
                   width = 3,
                   h2("Inputs"),
                   
                   fileInput(
                     inputId = "fileIn",
                     label = "Select a file with Rules",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                   ),
                   verbatimTextOutput("ruleCount"),
                   # column(4,
                   #actionButton("btUpload", "Use selected file", icon("file-upload"), width = "100%"),
                   # ),
                   # hr(),
                   selectizeInput(
                     'dropdown_subsets',
                     'Subsets',
                     c('Initial Data'),
                     options = NULL,
                     width = NULL
                   ),
                   #hr(),
                   fluidRow(
                     #   column(
                     #   6, actionButton("do", "Drop filter", icon("undo") , width = "100%")
                     # ),
                     column(
                       12,
                       actionButton("btSaveSubset", "Save Subset", icon("table") ,
                                    class="btn btn-primary btn-sm",
                                    width = "100%")
                     )),
                   hr(),
                   textInput("txt_filter", "Filter by String", value = ""),
                   
                   # Scatter Axis
                   fluidRow(column(
                     4,
                     selectInput('scatterX', 'X-axis', supconflif, selected = "support")
                   ),
                   column(
                     4,
                     selectInput('scatterY', 'Y-axis', supconflif, selected = "confidence")
                   ),
                   column(
                     4,
                     selectInput('scatterColor', 'Color', c("KPI", "FAULT_TYPE", "SET_SIZE"), selected = "confidence")
                   )
                   ), 
                   
                   
                   #Scatter plot
                   fluidRow(column(12,
                                   plotlyOutput("Scatter"))),
                   fluidRow(column(
                     12,
                     plotOutput(
                       "TargetBox",
                       #brush = brushOpts(id = "plot1_brush"),
                       click = "click_bar"
                     )
                   )),
                 ),
                 
                 # MainPanel for viz
                 
                 mainPanel(
                   width = 9,
                   tags$head(tags$style(HTML("
                            #ruleCount {
                            background-color: #FFFAFA;
                            }
                            #treeSuggestion {
                            background-color: #faf1f1;
                            }
                            #treeSummary {
                            background-color: #faf1f1;
                            }
                            #button {
                            background-color: red;
                            }
                             #container-fluid {
                            background-color: #faf1f1;
                            }
                            
                            
                            
                            "))),
                   
                   
                   # verbatimTextOutput("ruleCount"),
                   
                   # tabset for different vizs
                   wellPanel(
                     style = style_light,
                     tabsetPanel(
                       tabPanel(title = "Exploration",
                                br(),
                                fluidRow(
                                  column(
                                    3,
                                    fluidRow(verbatimTextOutput("treeSummary")),
                                    actionButton(inputId = 'btUseTreeSelection',
                                                label = "Filter by tree path",
                                                class="btn btn-primary btn-sm",
                                                width = "100%",
                                                icon ("cut")
                                                ),
                                    bucket_list(
                                      header = "Use Drag-n-Drop to build the hierarchy",
                                      add_rank_list(text = "Drag from here",
                                                    labels = tail(items, 5)),
                                      add_rank_list(
                                        text = "to hierarchy",
                                        labels = items[1:3],
                                        input_id = "bucketLHS"
                                      )
                                      
                                    ),
                                    verbatimTextOutput("treeSuggestion"),
                                    
                                    fluidRow(
                                      column(4,
                                             numericInput("treeDepth", min = 1, max = 4, value = 1, label = "Tree depth")
                                      ),
                                      column(6,
                                             selectizeInput(
                                               'dropdownHideStar',
                                               'Hide * in Sankey',
                                               c('yes', 'no'),
                                               options = NULL,
                                               width = NULL
                                             )
                                      ),
                                    ),
                                    
                                    plotOutput("supportTree")
                                  ),
                                    
                                    
                                    
                                  # ),
                                  column(
                                    9,
                                    fluidRow(
                                      column(12,
                                    h4("Dynamic Tree by Fault count", align = "center"),
                                    collapsibleTreeOutput("collapsTree"),
                                             )
                                    # ,
                                    #   column(3,
                                    #      plotlyOutput("aggregationLevelsBar"),
                                    #          )
                                      
                                    ),
                                    fluidRow(
                                      column(6,
                                    plotOutput("sankeyPlot")
                                             
                                             ),
                                      column(6,
                                         plotlyOutput("aggregatedFaultsHeat")
                                        
                                      )
                                      
                                      
                                    )
                                    # hr(),
                                    # h4("Sankey Plot by Fault count", align = "center"),
                                    
                                    
                                  ),
                                
                                  
                                  )
                                
                                
                                ),
                       
                       ####### END TAB PANEL #######
                       tabPanel(title = "DecesionTrees",
                                fluidRow(
                                  column(
                                    6,
                                    style = "background-color:#ff7b7b;",
                                    h2("Regression Tree", align = "center"),
                                    plotOutput("decisionTreeRegression"),
                                    br()
                                  ),
                                  column(
                                    6,
                                    style = "background-color:#ff5252;",
                                    h2("Classification Tree", align = "center"),
                                    plotOutput("decisionTreeClass"),
                                    br()
                                  )
                                )),
                       
                       
                       ####### END TAB PANEL #######
                       
                       tabPanel(
                         title = "Data table",
                         DT::dataTableOutput("fileTable2"),
                         DT::dataTableOutput("tableSelected"),
                         hr(),
                         actionButton(
                           'btFilterTable',
                           label = "Filter by selected rules",
                           icon = icon("exchange"),
                           class="btn btn-primary btn-lg",
                           width = "100%"
                           # class = "btn-success"
                         )
                       ),
                       
                       ####### END TAB PANEL #######
                       
                       tabPanel(title = "Plots",
                                h2("Overview and compare subsets"),
                                fluidRow(column(
                                  5,
                                  selectizeInput(
                                    'dropdown_plots1',
                                    'Subsets',
                                    c('Initial Data'),
                                    options = NULL,
                                    width = NULL
                                  )
                                ),
                                column(
                                  5,
                                  selectizeInput(
                                    'dropdown_plots2',
                                    'Subsets',
                                    c('Initial Data'),
                                    options = NULL,
                                    width = NULL
                                  )
                                ),
                                column(2,
                                       selectizeInput(
                                         'dropdown_absRel',
                                         'Scale',
                                         c('relative', 'absolute'),
                                         options = NULL,
                                       )
                                       
                                )
                                ),
                                # plots
                                hr(),
                                
                                h4("From Selection 1 generated Rules"),
                                visNetworkOutput("rules_graph"),
                                hr(),
                                verbatimTextOutput("plots_info"),
                                hr(),
                                plotlyOutput("plots_1"),
                                plotlyOutput("plots_9"), # Aggregation level plot
                                
                                hr(),
                                fluidRow(
                                  column(6,
                                         plotlyOutput("plots_2")
                                  ),
                                  column(6,
                                         plotlyOutput("plots_3")
                                  )
                                ),
                                hr(),
                                fluidRow(
                                  column(6,
                                         plotlyOutput("plots_4")
                                  ),
                                  column(6,
                                         plotlyOutput("plots_5")
                                  )
                                ),
                                hr(),
                                fluidRow(
                                  column(6,
                                         plotlyOutput("plots_6")
                                  ),
                                  column(6,
                                         plotlyOutput("plots_7")
                                  )
                                ),
                                
                                plotlyOutput("plots_8"),
                                
                                # plotlyOutput("plots_9"),
                                hr()
                                
                                
                       )
                       
                       ####### END TAB PANEL #######
                       
                       
                     ) # END OF TABSET PANEL
                   ) # END OF WELL PANEL
                 ) # END OF MAIN PANEL
               ) # END OF SIEDEBAR LAYOUT
             ), # END OF TAB PANEL - NAV 
             tabPanel("About",
                      
                      fluidRow(column(
                        12,
                        style = style_light,
                        h1("Subgroup X-plorer"),
                        # h2('Explaination'),
                        p(
                          'This tool was developed as a practical part of Benjamin Wuthe\'s master\'s thesis in the Business Informatics program at HTW Berlin.'
                        ),
                        hr(),
                        h3('What is'),
                        p('Subgroup explorer'),
                        h3('How to use'),
                        p('Lorem Ipsum\n'),
                        hr(),
                        hr()
                      ))
             ) # END OF TABPANEL
             
  ) # END OF NAV BAR
  
) # END OF UI

