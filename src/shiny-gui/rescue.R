########################################################################################################
#Drone rescue with Map UI
########################################################################################################
#NOTES:
#Save config button
#Concorde can not be installed for shiny?
#Boat to Boat travel may not be possible if not enough fuel remains in the drone
#In this case you may need to go back to a fuel depot before flying to the next boat directly
#
#
########################################################################################################
#When ui and server in the same file run with:
#myApp <- shinyApp(ui, server)
#To broadcast it on the net!!!
#runApp(myApp,host="0.0.0.0",port=8181)
########################################################################################################
#DO NOT FORGET TO RUN setRepositories() from the console before publishing
#Clean the environment
remove(list = ls())

source("global.R")
########################################################################################################

shinyApp(ui, server)


########################################################################################################
