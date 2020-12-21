server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hst <- hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  output$fileTable <- renderTable(
    {
      req(input$fileIn)
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- read.csv(input$fileIn$datapath,
                         sep = ",")
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
    }
  ) # end fileTable
} # end server