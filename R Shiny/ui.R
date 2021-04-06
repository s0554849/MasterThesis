library(shinythemes)
library(bslib)
library(shinyWidgets)
library(plotly)
library(sortable)
library(r2d3)
library(collapsibleTree)


items <-
  c(
    "AGE",
    "MODEL",
    "DEALERSHIP"	,
    "CUSTOMER_TYPE",
    "USER_CUSTOMISED",
    "COUNTRY",
    "GEO_TYPE",
    "FAULT_TYPE"
  )
supconflif <- c('support', 'confidence', 'lift')

style_light <- "background-color: #FFFAFA;"
style_dark <-"#faf1f1"

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
                   fluidRow(column(
                     6, actionButton("do", "Drop filter", icon("undo") , width = "100%")
                   ),
                   column(
                     6,
                     actionButton("btSaveSubset", "Save Subset", icon("table") , width = "100%")
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
                       brush = brushOpts(id = "plot1_brush"),
                       click = "click_bar"
                     )
                   )),
                 ),
                 
                 # MainPanel for viz
                 
                 mainPanel(
                   width = 9,
                   verbatimTextOutput("ruleCount"),
                   
                   
                   # tabset for different vizs
                   wellPanel(
                     style = style_light,
                     tabsetPanel(
                       tabPanel(title = "Tree",
                                fluidRow(
                                  column(
                                    4,
                                    fluidRow(verbatimTextOutput("treeSummary")),
                                    bucket_list(
                                      header = "Use Drag-n-Drop to build the hierarchy",
                                      add_rank_list(text = "Drag from here",
                                                    labels = tail(items, 6)),
                                      add_rank_list(
                                        text = "to hierarchy",
                                        labels = items[1:2],
                                        input_id = "bucketLHS"
                                      )
                                    
                                      
                                    ),
                                    verbatimTextOutput("treeSuggestion"),
                                    sliderInput("treeDepth", min = 1, max = 4, value = 1, label = "Tree depth"),
                                    plotOutput("supportTree")
                                    
                                  ),
                                  column(
                                    8,
                                    h4("Dynamic Tree by Fault count", align = "center"),
                                    collapsibleTreeOutput("collapsTree"),
                                    hr(),
                                    h4("Sankey Plot by Fault count", align = "center"),
                                    
                                    plotOutput("sankeyPlot")
                                    
                                  )
                                )),
                       
                       ####### END TAB PANEL #######
                       tabPanel(title = "DecesionTree",
                                fluidRow(
                                  column(
                                    6,
                                    style = "background-color:#8ad3ff;",
                                    h2("Regression Tree", align = "center"),
                                    plotOutput("decisionTreeRegression"),
                                    br()
                                  ),
                                  column(
                                    6,
                                    style = "background-color:#b6fffd;",
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
                         actionButton(
                           'btFilterTable',
                           label = "Filter by selected rules",
                           icon = icon("exchange"),
                           class = "btn-success"
                         )
                       ),
                       
                       ####### END TAB PANEL #######
                       
                       tabPanel(title = "Plots",
                                h2("Overview and compare subsets"),
                                fluidRow(column(
                                  6,
                                  selectizeInput(
                                    'dropdown_plots1',
                                    'Subsets',
                                    c('Initial Data'),
                                    options = NULL,
                                    width = NULL
                                  )
                                ),
                                column(
                                  6,
                                  selectizeInput(
                                    'dropdown_plots2',
                                    'Subsets',
                                    c('Initial Data'),
                                    options = NULL,
                                    width = NULL
                                  )
                                )),
                                # plots
                                
                                plotlyOutput("plots_1"),
                                
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
