########################################################################################################
#Set the directory for the code and output
########################################################################################################

myWorkDir <- "."
setwd(myWorkDir)

########################################################################################################
#Clean the environment
remove(list = ls())
########################################################################################################


myDataDir  <- "./data"
myResultDir  <- "./result"

########################################################################################################
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(htmlwidgets)
library(igraph)
library(gtools)
library(magrittr)
library(sf)
library(sp)
library(leaflet)
library(mapview)
library(rgdal)
library(geosphere)
library(stringr)
library(readr)
library(svgPanZoom) #For zooming the plot
library(svglite)
#library(gissr)
library(tictoc)
library(GA)
library(tidyverse)
library(concaveman)
library(TSP)
########################################################################################################

########################################################################################################
source("my_functions.R")
########################################################################################################





########################################################################################################

########################################################################################################
# Global function to reset all global variables
########################################################################################################

my_reset_all_globals <- function(){
  
  
  #########################################
  #Region polygon
  #########################################
  #Boolean for marking the begin of selecting polygon vertices 
  #Initially the poly is presented over Sea of Marche anyway
  polyBEGIN <<- FALSE
  #Global polygon for the demo region
  regionPoly <<-  st_polygon(list())
  regionPolyBBox <<- st_bbox(regionPoly)
  regionPolyArea <<- 0
  
  regionPolyDF <<- NULL
  regionVtxMtx <<- NULL
  #########################################
  #Boats
  #########################################
  #Boolean for marking the begin of selecting Boat points
  #Initially the bs selection is open
  #Active
  btBEGIN <<- FALSE
  nBT <<- 0 
  BTList <<- list()
  BTMtx <<- NULL
  
  #Tri
  TrinBT <<- 0 
  TriBTList <<- list()
  TriBTMtx <<- NULL
  
  
  #Sq
  SqnBT <<- 0 
  SqBTList <<- list()
  SqBTMtx <<- NULL
  
  #Boats that can charge the drone
  #Assign it randomly?
  BTwithCharging <<- list()
  TriBTwithCharging <<- list()  
  SqBTwithCharging <<- list()
  
  
  #Boat to Depot data structures
  #Active
  BT2DTlinesMTX  <<- NULL
  BT2DTMtx <<- NULL
  BT2DTAdj <<- NULL
  BT2DTedgeW <<- NULL
  BT2DTGrayEdge <<- NULL
  BT2DTRedEdge <<- NULL
  
  #Tri
  BT2TriDTlinesMTX  <<- NULL
  BT2TriDTMtx <<- NULL
  BT2TriDTAdj <<- NULL
  BT2TriDTedgeW <<- NULL
  BT2TriDTGrayEdge <<- NULL
  BT2TriDTRedEdge <<- NULL
  
  #Sq
  BT2SqDTlinesMTX  <<- NULL
  BT2SqDTMtx <<- NULL
  BT2SqDTAdj <<- NULL
  BT2SqDTedgeW <<- NULL
  BT2SqDTGrayEdge <<- NULL
  BT2SqDTRedEdge <<- NULL
  
  #Boat to Boat data structures
  #Active
  BT2BTlinesMTX  <<- NULL
  BT2BTMtx <<- NULL
  BT2BTAdj <<- NULL
  BT2BTedgeW <<- NULL
  
  
  #Tri
  TriBT2BTlinesMTX  <<- NULL
  TriBT2BTMtx <<- NULL
  TriBT2BTAdj <<- NULL
  TriBT2BTedgeW <<- NULL
  
  
  #Sq
  SqBT2BTlinesMTX  <<- NULL
  SqBT2BTMtx <<- NULL
  SqBT2BTAdj <<- NULL
  SqBT2BTedgeW <<- NULL
  
  
  
  
  #max number of boats at the same time
  maxBoats <<- 1000
  
  
  
  #########################################
  #BSs
  #########################################
  #Boolean for marking the begin of selecting BS points
  #BS must be in the region!!!!
  bsBEGIN <<- FALSE
  nBS <<- 0 
  BSList <<- list()
  BSMtx <<- NULL
  
  
  # #BS point[1] at: Lat: 43.631 Lng: 13.5
  # 
  # nBS <<- 1
  # #enable("deployDepots")
  # 
  # BSList[[nBS]] <<- st_point(c(13.5, 43.631))
  # BSMtx <<- rbind(BSMtx, c(13.5, 43.631))
  
  
  
  
  #########################################
  #Depots: Active, Tri and Sq grid
  #########################################
  #Active
  activeGridType <<- "tri"
  
  nDT <<- 0
  DTList <<- list()
  DTMtx <<- NULL
  D2DlinesMTX <<- NULL
  depotPolygons <<- st_polygon(list())
  
  #Depot config mtx
  depotPositions <<- NULL
  depotAdj <<- NULL
  edgeW <<- NULL
  
  
  #For Triangular Grid
  TriDTReady <<- FALSE
  nTriDT <<- 0
  TriDTList <<- list()
  TriDTMtx <<- NULL
  TriD2DlinesMTX <<- NULL
  TriDepotPolygons <<- st_polygon(list())
  
  #Depot config mtx
  TriDepotPositions <<- NULL
  TriDepotAdj <<- NULL
  TriEdgeW <<- NULL
  
  #For Square Grid
  SqDTReady <<- FALSE
  nSqDT <<- 0
  SqDTList <<- list()
  SqDTMtx <<- NULL
  SqD2DlinesMTX <<- NULL
  SqDepotPolygons <<- st_polygon(list())
  
  #Depot config mtx
  SqDepotPositions <<- NULL
  SqDepotAdj <<- NULL
  SqEdgeW <<- NULL
  
  #########################################
  #Global counters
  #########################################
  #Total BS and Depots
  nBSandDepot <<- nBS + nDT
  
  nBSandTriDepot <<- nBS + nTriDT
  nBSandSqDepot <<- nBS + nSqDT
  
  
  #Total vertices for the graph. BSs + Depots + Boats
  #Active
  nVertex <<- nBS + nDT + nBT
  #vertexPositions <<- NULL
  VTXMtx <<- NULL
  
  #Tri
  nTriVertex <<- nBS + nTriDT + nBT
  #TriVertexPositions <<- NULL
  TriVTXMtx <<- NULL
  
  #Sq
  nSqVertex <<- nBS + nSqDT + nBT
  #SqVertexPositions <<- NULL
  SqVTXMtx <<- NULL
  
  
  #########################################
  
  
  #########################################
  #Drone pars
  #########################################
  min_drone_range <<- 10000 #5000m
  max_drone_range <<- 30000 #20000m
  rangeDelta <<- 500 #500m for discovering the proximity stuff
  rescueDist <<- floor(((max_drone_range + min_drone_range)/2) / 2)
  
  
  #ceiling((min_drone_range + floor((max_drone_range - min_drone_range)/2)) / 2) #+ rangeDelta
  #DEBUG variable
  drange <<- 0
  
  
  ####################################################################################
  #The graph stuff
  ####################################################################################
  #Active
  gBSandDT <- make_empty_graph() #Only Base Stat and Depots
  gRedGray <- make_empty_graph()
  gGray  <- make_empty_graph()
  gRedGrayYellow  <- make_empty_graph()
  
  #Tri
  gTriBSandDT <- make_empty_graph() 
  gTriRedGray <- make_empty_graph()
  gTriGray  <- make_empty_graph()
  gTriRedGrayYellow  <- make_empty_graph()
  
  #Sq
  gSqBSandDT <- make_empty_graph() 
  gSqRedGray <- make_empty_graph()
  gSqGray  <- make_empty_graph()
  gSqRedGrayYellow  <- make_empty_graph()
  
  theTriAVGpathLen <<- 0
  
  theSqAVGpathLen  <<- 0
  
}













########################################################################################################
# Here set variables
########################################################################################################


########################################################################################################
#Global vars
########################################################################################################

#Weights for optimization
distWeight <- 3.0
plWeight <- 1.0
awgWaitingWeight <- 0
numChargingWeight <- 0






#Projection string for leaflet objects
#EPSG:3395 (World Mercator)
#CRS (Web Mercator, 3857).

#myCRS <- "+init=epsg:4326"
#myCRS  <- 3395
distBetween2Deg <- geosphere::distGeo(c(0, 0), c(1, 0))
nPrecDigit <- 3

#########################################
#Region polygon
#########################################
#Boolean for marking the begin of selecting polygon vertices 
#Initially the poly is presented over Sea of Marche anyway
polyBEGIN <- FALSE
#Global polygon for the demo region
regionPoly <-  st_polygon(list())
regionPolyBBox <- st_bbox(regionPoly)
regionPolyArea <- 0


#########################################
#Boats
#########################################
#Boolean for marking the begin of selecting Boat points
#Initially the bs selection is open

btBEGIN <- FALSE
nBT <- 0 
BTList <- list()
BTMtx <- NULL


#Tri
TrinBT <- 0 
TriBTList <- list()
TriBTMtx <- NULL


#Sq
SqnBT <- 0 
SqBTList <- list()
SqBTMtx <- NULL

#Boats that can charge the drone
#Assign it randomly?
BTwithCharging <- list()
TriBTwithCharging <- list()  
SqBTwithCharging <- list()



#Boat to Depot data structures
#Active
BT2DTlinesMTX  <- NULL
BT2DTMtx <- NULL
BT2DTAdj <- NULL
BT2DTedgeW <- NULL
BT2DTGrayEdge <- NULL
BT2DTRedEdge <- NULL

#Tri
BT2TriDTlinesMTX  <- NULL
BT2TriDTMtx <- NULL
BT2TriDTAdj <- NULL
BT2TriDTedgeW <- NULL
BT2TriDTGrayEdge <- NULL
BT2TriDTRedEdge <- NULL

#Sq
BT2SqDTlinesMTX  <- NULL
BT2SqDTMtx <- NULL
BT2SqDTAdj <- NULL
BT2SqDTedgeW <- NULL
BT2SqDTGrayEdge <- NULL
BT2SqDTRedEdge <- NULL

#Boat to Boat data structures
#Active
BT2BTlinesMTX  <- NULL
BT2BTMtx <- NULL
BT2BTAdj <- NULL
BT2BTedgeW <- NULL



#Tri
TriBT2BTlinesMTX  <- NULL
TriBT2BTMtx <- NULL
TriBT2BTAdj <- NULL
TriBT2BTedgeW <- NULL


#Sq
SqBT2BTlinesMTX  <- NULL
SqBT2BTMtx <- NULL
SqBT2BTAdj <- NULL
SqBT2BTedgeW <- NULL


#max number of boats at the same time
maxBoats <- 1000

#The angle that boats are divided into groups for scanning:
BTGroupSectorAngle <- 30

#cscan sectors
sectors <- seq(-180,180,BTGroupSectorAngle)
nSectors <- length(sectors)

#########################################
#BSs
#########################################
#Boolean for marking the begin of selecting BS points
#For the time being let say just 1 BS
#Initially the bs selection is open
#BS must be in the region!!!!
bsBEGIN <- TRUE
nBS <- 0 
BSList <- list()
BSMtx <- NULL


# #########################################
# #START WITH SINGLE S FOR EXPERIMENTS
# #BS point[1] at: Lat: 43.631 Lng: 13.5
# 
# nBS <- 1
# #enable("deployDepots")
# 
# BSList[[nBS]] <- st_point(c(13.5, 43.631))
# BSMtx <- rbind(BSMtx, c(13.5, 43.631))
# #########################################



#########################################
#Depots: Active, Tri and Sq grid
#########################################
#Active
activeGridType <- "tri"
nDT <- 0
DTList <- list()
DTMtx <- NULL
D2DlinesMTX <- NULL
depotPolygons <- st_polygon(list())

#Depot config mtx
depotPositions <- NULL
depotAdj <- NULL
edgeW <- NULL


#For Triangular Grid
TriDTReady <- FALSE
nTriDT <- 0
TriDTList <- list()
TriDTMtx <- NULL
TriD2DlinesMTX <- NULL
TridepotPolygons <- st_polygon(list())

#Depot config mtx
TridepotPositions <- NULL
TridepotAdj <- NULL
TriedgeW <- NULL

#For Square Grid
SqDTReady <- FALSE
nSqDT <- 0
SqDTList <- list()
SqDTMtx <- NULL
SqD2DlinesMTX <- NULL
SqdepotPolygons <- st_polygon(list())

#Depot config mtx
SqdepotPositions <- NULL
SqdepotAdj <- NULL
SqedgeW <- NULL

#########################################
#Global counters
#########################################
#Total BS and Depots
nBSandDepot <- nBS + nDT

nBSandTriDepot <- nBS + nTriDT
nBSandSqDepot <- nBS + nSqDT


#Total vertices for the graph. BSs + Depots + Boats
#Active
nVertex <- nBS + nDT + nBT
#vertexPositions <- NULL
VTXMtx <- NULL

#Tri
nTriVertex <- nBS + nTriDT + nBT
#TrivertexPositions <- NULL
TriVTXMtx <- NULL

#Sq
nSqVertex <- nBS + nSqDT + nBT
#SqvertexPositions <- NULL
SqVTXMtx <- NULL


#########################################


#########################################
#Drone pars
#########################################
min_drone_range <- 10000 #5000m
max_drone_range <- 30000 #20000m
rangeDelta <- 500 #500m for discovering the proximity stuff
rescueDist <- floor(((max_drone_range + min_drone_range)/2) / 2)


#ceiling((min_drone_range + floor((max_drone_range - min_drone_range)/2)) / 2) #+ rangeDelta
#DEBUG variable
drange <- 0

####################################################################################





####################################################################################
#The graph stuff
####################################################################################
#Active
gBSandDT <- make_empty_graph()
gRedGray <- make_empty_graph()
gGray  <- make_empty_graph()
gRedGrayYellow  <- make_empty_graph()

#Tri
gTriBSandDT <- make_empty_graph()
gTriRedGray <- make_empty_graph()
gTriGray  <- make_empty_graph()
gTriRedGrayYellow  <- make_empty_graph()

#Sq
gSqBSandDT <- make_empty_graph()
gSqRedGray <- make_empty_graph()
gSqGray  <- make_empty_graph()
gSqRedGrayYellow  <- make_empty_graph()

# 
# #xxxgGrayOnly <- make_empty_graph()
# 
# #TODO: THINK ABOUT THEM
# gWithoutRed <- make_empty_graph()
# gWithRedandGray <- make_empty_graph()
# gWithYellow <- make_empty_graph()
# gWithBSDT <- make_empty_graph()
# gWithBSDTBT <- make_empty_graph()

#Scale for the actual Geo coordinates for the graph
myGraphScale <- 10


theTriAVGpathLen <- 0

theSqAVGpathLen  <- 0
####################################################################################


























####################################################################################
#########################################
#For initial default region
#########################################
#Read KML that is created from Google Earth
#For Marche sea
#regionVtxMtx <- maptools::getKMLcoordinates(kmlfile="gridMarcheSea.kml", ignoreAltitude=T)
regionVtxMtx <- maptools::getKMLcoordinates(kmlfile="SeaMarcheSmall.kml", ignoreAltitude=T)
regionPolyDF <- VtxMtx2SpatPolyDF(regionVtxMtx)
cat("Global length:", length(regionPolyDF@polygons[[1]]@Polygons[[1]]@coords[,1])-1,"\n")
#regionVtxMtx <<- rbind(regionVtxMtx, regionVtxMtx[1,])
regionPoly <-  st_polygon(regionVtxMtx)
#regionPoly <- st_transform(regionPoly, 3857)
regionPolyBBox <- st_bbox(regionPoly)
#Find the bbox and overlay the map
####################################################################################
frameBBX <- st_bbox(regionPolyDF)
framePoly <- Polygon(rbind(c(frameBBX$xmin,frameBBX$ymin),c(frameBBX$xmin,frameBBX$ymax),
                           c(frameBBX$xmax,frameBBX$ymax), c(frameBBX$xmax,frameBBX$ymin),
                           c(frameBBX$xmin,frameBBX$ymin)))
framePolys <- Polygons(list(framePoly),1)

#regionBBoxPoly <- SpatialPolygons(list(framePolys), proj4string=sp::CRS("+init=epsg:3857"))
regionBBoxPoly <- SpatialPolygons(list(framePolys), proj4string=sp::CRS("+init=epsg:3857"))
regionPolyArea <- st_area(regionPoly)
mapH <- lonlat2m(regionPolyBBox$xmin,regionPolyBBox$ymin,regionPolyBBox$xmin,regionPolyBBox$ymax)
mapW <- lonlat2m(regionPolyBBox$xmin,regionPolyBBox$ymin,regionPolyBBox$xmax,regionPolyBBox$ymin)
cat("RegionBBox width:",round(mapW/1000,2), "km - height:",round(mapH/1000,2) ," km\n")
cat("Region area is",areaPolygon(regionPoly[[1]]),"meter^2\n")
####################################################################################







####################################################################################
#Custom igraph vertex shape for depots
####################################################################################


myVertexShapelwd <- 0.3
####################################################################################
#diamonds for depots
####################################################################################
mydepot <- function(coords, v = NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  # vertex.frame.color <- params("vertex", "frame.color")
  # if (length(vertex.frame.color) != 1 && !is.null(v)) {
  #   vertex.frame.color <- vertex.frame.color[v]
  # }
  # vertex.frame.width <- params("vertex", "frame.width")
  # if (length(vertex.frame.width) != 1 && !is.null(v)) {
  #   vertex.frame.width <- vertex.frame.width[v]
  # }
  
  symbols(x = coords[, 1], y = coords[, 2], bg = vertex.color,
          stars = cbind(1.3*vertex.size, vertex.size, 1.3*vertex.size, vertex.size),
          add = TRUE, inches = FALSE, cex = 0.5, lwd = myVertexShapelwd)
}
add_shape("myDT", clip = shapes("circle")$clip, plot = mydepot)

####################################################################################
#Square for BSs
####################################################################################
mybs <- function(coords, v = NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  
  symbols(x = coords[, 1], y = coords[, 2], bg = vertex.color,
          squares = 2.5*vertex.size,
          add = TRUE, inches = FALSE, cex = 0.5, lwd = myVertexShapelwd)
}
add_shape("myBS", clip = shapes("circle")$clip, plot = mybs)



####################################################################################
#circle for the boat
####################################################################################
mybt <- function(coords, v = NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  
  symbols(x = coords[, 1], y = coords[, 2], bg = vertex.color,
          circles =  vertex.size,
          add = TRUE, inches = FALSE, cex = 0.5, lwd = myVertexShapelwd)
}
add_shape("myBT", clip = shapes("circle")$clip, plot = mybt)



####################################################################################






####################################################################################
#msg buffer to print
####################################################################################
my_text <- ""



#Text buffer box
textBuf <- ""

####################################################################################




####################################################################################
#Colors for the graph
####################################################################################
boatCol <- blues9[7]
boatWithChargerCol <- "green"

depotCol <- "green"
depotEdgeCol <- "green"

redEdgeCol <- "red"
grayEdgeCol <- "gray"
yellowEdgeCol <- "yellow"
BSCol <- "orange"
rescuePathCol <- "lightblue"
####################################################################################




####################################################################################
#icons for the map
####################################################################################
myBSIcon <- makeIcon(
  iconUrl = "bs-tr.png",
  iconWidth = 48, iconHeight = 64,
  iconAnchorX = 48, iconAnchorY = 64,
)




myDTIcon <- makeIcon(
  iconUrl = "dt-tr.png",
  iconWidth = 14, iconHeight = 25,
  iconAnchorX = 14, iconAnchorY = 25,
)


myBTIcon <- makeIcon(
  iconUrl = "bt-tr.png",
  iconWidth = 24, iconHeight = 24,
  iconAnchorX = 24, iconAnchorY = 24,
)
####################################################################################


####################################################################################
# concaveTSP variables
####################################################################################

cr <- 0.9
#ATTENTION concaveman rounds numbers to 4 prec digits
coordDigitPrec <- 4




####################################################################################
# Sim variables
####################################################################################


xTSPmethods = c("nearest_insertion",
            "farthest_insertion",
            "cheapest_insertion",
            "arbitrary_insertion",
            "nn",
            "repetitive_nn",
            "two_opt")


####################################################################################
# Statistics variables

resultFileName <- paste0("results-01.csv")
resultFile <- paste(myResultDir,resultFileName, sep = "/")
cat("Saving benchmark results to", resultFile,"\n")


#Counter for boatpos file
boatPosCNT <- 0

boatPosTXT <- str_pad(boatPosCNT, 4, pad = "0")

boatPosFileName <- paste0("boatPos-",boatPosTXT,".csv")
boatPosFile <- paste(myDataDir,boatPosFileName, sep = "/")
cat("Saving boat configuration to", boatPosFile,"\n")



#Recreate the results file!!!

#theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
theColNames <- "GridType, Algo, NBoat, Cost, AWD, NCharging, NCS, Time, BtPosFName, BPerm, SPath"
cat(theColNames,"\n",file=resultFile,append=FALSE)
########################################################################################################
# End variables
########################################################################################################




