# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# To Deploy App on Shiny.io 
# install.packages('rsconnect')
# library(rsconnect)
# rsconnect::setAccountInfo(name='s0554849', 
#                           token='XXX', 
#                           secret='XXX')
# rsconnect::deployApp('/Users/benjaminwuthe/git/MasterThesis/R Shiny')

library(shiny)

# Define UI and Server for application in separate files 
source('ui.R', local = TRUE)
source(server.R)
# Run the application 
shinyApp(ui = ui, server = server)
