# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)

# Define UI and Server for application in seperate files 
source('ui.R', local = TRUE)
source(server.R)

# this is cool






# Run the application 
shinyApp(ui = ui, server = server)
