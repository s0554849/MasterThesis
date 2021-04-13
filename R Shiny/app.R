# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# To Deploy on Shiny.io 
#install.packages('rsconnect')
# library(rsconnect)
# rsconnect::setAccountInfo(name='s0554849', token='20BC445FCC06BD48656502B4F84A8513', secret='hJmkQReCsyaWZLfOQvzx2eLVHWuHWntwBm42pcIx')
# rsconnect::deployApp('/Users/benjaminwuthe/git/MasterThesis/R Shiny')


library(shiny)

# Define UI and Server for application in seperate files 
source('ui.R', local = TRUE)
source(server.R)
# Run the application 
shinyApp(ui = ui, server = server)
