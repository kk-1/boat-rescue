
########################################################################################################
# Define server logic
########################################################################################################
server <- function(input, output, session) {
  ##############################################################################################
  #TODO Cehck to see if it is a good idea to have such a function as the following one!!!
  ##############################################################################################
  #Function that returns the sp. Null if there is no path!!!
  #If checkRedGray is set then it check possible RedGray path
  #The path is the sequence of vertex numbers and the last vertex can be the boat or depot
  ##############################################################################################
  findSP <- function(theGraph, srcVtx, destVtx, checkRedGray) {
    
    if (checkRedGray == 1){
      #Check the existence of RedGray path
      
    }else{
      #Find the normal sp
    }
    
    
  }
  
  
  
  # Create a separate map object for printing - same properties as what's on the screen
  user_created_map <- reactive({
    m = mapToSave$current %>%
      setView(lng = input$mymap_center$lng, lat = input$mymap_center$lat, 
              zoom = input$mymap_zoom)
    
    m
  })
  
  
  
  
  
  
  
  
  
  observeEvent(input$saveMap, {
    mapshot(user_created_map(), file=paste0(getwd(), '/exported_map.png'))
  })
  
  
  ########################################################################################################
  #Initially disable these
  ########################################################################################################
  #The initial polygon is already there so disable startPoly and startBT
  updateSwitchInput(
    session = session,
    inputId = "startPoly",
    disabled = TRUE,
    value = FALSE,
  )
  
  updateSwitchInput(
    session = session,
    inputId = "startBT",
    disabled = TRUE,
    value = FALSE,
  ) 
  
  updateSwitchInput(
    session = session,
    inputId = "startBS",
    disabled = FALSE,
    value = TRUE
  )
  
  updateSwitchInput(
    session = session,
    inputId = "cov",
    disabled = TRUE,
    value = FALSE
  )
  
  
  updateRadioButtons(
    session = session, 
    inputId = "grid",
    selected = "tri"
  )
  
  activeGridType <<- "tri"
  
  #TODO: Later try to enable them and make changes necessary to update plots paths etc...
  disable("rndBoats")
  disable("rnd_boat_number")
  disable("drone_range")
  disable("depot_spacing")
  
  disable("closePolygon")
  
  disable("deployTriDepots")
  disable("deploySqDepots")
  
  disable("rescueRedGray1by1")
  disable("rescueGray1by1")
  disable("permRedGray")
  disable("permGray")
  disable("clookRedGray")
  disable("clookGray")
  disable("gaPermRedGray")
  disable("gaSectorRedGray")
  
  bsBEGIN <<- TRUE
  btBEGIN <<- FALSE
  polyBEGIN <<- FALSE
  
  #Reactive values
  mapToSave <- reactiveValues()
  
  
  # Create a reactive value to store the graph for plotting
  myGraph <- reactiveValues()
  myGraph$value <- make_empty_graph()
  
  ########################################################################################################
  
  
  
  
  
  
  
  
  ########################################################################################################
  
  my_clear_msg <- function(){
    my_text<-" "
    output$msg1<- renderText({ my_text })
  }
  
  ########################################################################################################
  
  
  
  
  ########################################################################################################
  observeEvent(input$edgeFilter,{ 
    
    if(input$edgeFilter=="gr"){
      myGraph$value <- gGray
    }else if(input$edgeFilter=="rg"){
      myGraph$value <- gRedGray
    }else if(input$edgeFilter=="rgy"){
      myGraph$value <- gRedGrayYellow
    }
    
  })
  
  ########################################################################################################
  
  
  
  
  
  
  
  
  ########################################################################################################
  
  setButtonsAfterDeployDepots <- function(){
    #Disable startBS
    updateSwitchInput(
      session = session,
      inputId = "startBS",
      disabled = TRUE,
      value = FALSE,
    )
    bsBEGIN <<- FALSE
    
    #Disable startPoly 
    updateSwitchInput(
      session = session,
      inputId = "startPoly",
      disabled = TRUE, 
      value = FALSE,
    )
    polyBEGIN <<- FALSE
    
    #Enable Boat select
    updateSwitchInput(
      session = session,
      inputId = "startBT",
      disabled = FALSE, 
      value = TRUE,
    )
    btBEGIN <<- TRUE
    
    
    updateSwitchInput(
      session = session,
      inputId = "cov",
      disabled = FALSE,
      value = TRUE
    )
    
    enable("rndBoats")
    enable("rnd_boat_number")
    
    disable("closePolygon")
    disable("deployTriDepots")
    disable("deploySqDepots")
    
  }
  ########################################################################################################  
  
  
  
  
  
  #######################################################################################################  
  #HERE set the BT2DTGrayEdge in activateDepots
  activateDepots <- function(gridType){
    
    
    if (gridType == "tri") { 
      
      updateRadioButtons(
        session = session, 
        inputId = "grid",
        selected = "tri"
      )
      
      activeGridType <<- "tri"
      
      DTMtx <<- TriDTMtx
      depotPolygons <<- TriDepotPolygons
      depotPositions <<-  TridepotPositions 
      
      nBT <<-TrinBT
      nDT <<- nTriDT 
      
      
      #VertexPositions <<- TriVertexPositions
      VTXMtx <<-  TriVTXMtx
      
      nBSandDepot <<- nBSandTriDepot
      nVertex <<- nTriVertex
      
      
      depotAdj <<- TriDepotAdj
      
      edgeW <<-  TriEdgeW
      D2DlinesMTX <<-  TriD2DlinesMTX
      
      DTList <<- TriDTList
      
      #Boat stuff
      BTList <<- TriBTList
      BTMtx <<- TriBTMtx
      BTwithCharging <<-  TriBTwithCharging
      
      BT2BTlinesMTX  <<-  TriBT2BTlinesMTX
      BT2BTMtx <<- TriBT2BTMtx
      BT2BTAdj <<- TriBT2BTAdj 
      BT2BTedgeW <<-  TriBT2BTedgeW 
      
      
      #Boat to Depot data structures
      #Active
      BT2DTlinesMTX  <<-  BT2TriDTlinesMTX
      BT2DTMtx <<- BT2TriDTMtx
      BT2DTAdj <<- BT2TriDTAdj
      BT2DTedgeW <<- BT2TriDTedgeW
      BT2DTGrayEdge <<- BT2TriDTGrayEdge
      BT2DTRedEdge <<- BT2TriDTRedEdge
      
      
      
      #Active
      gBSandDT  <<- gTriBSandDT 
      gRedGray  <<-  gTriRedGray
      gGray   <<- gTriGray
      gRedGrayYellow  <<- gTriRedGrayYellow
      
      myGraph$value <- gTriRedGray
      
      allSPfromBS <- shortest_paths(gTriRedGray, from = 1, to = V(gTriRedGray), output = "both")
      eLen <- length(allSPfromBS$epath)
      #The first  one is to BS itself!!!
      eSum <- 0
      eCNT <- 0
      for (eIDX in (2:eLen)){
        eSum <- eSum + sum(allSPfromBS$epath[[eIDX]]$weight)
        eCNT <- eCNT + length(allSPfromBS$epath[[eIDX]])
      }
      
      avgSPFromBS <- eSum / (eLen - 1)
      avgeCNTFromBS <- eCNT / (eLen - 1)
      
      theTriAVGpathLen <<- mean_distance(gTriRedGray)
      
      dRange <- 20000
      cat("******> TriDepot activated! The Grid has AVG Path length of:",theTriAVGpathLen,"\n",
          "Drone range:", dRange,"\n",
          "From my CNT",avgeCNTFromBS,"-", avgSPFromBS, "m drone range unit:", 
          round(avgSPFromBS/dRange, 3),"\n")
      
      # *** Tri grid
      # BS + TriDepots =  25 BS =  1 TriDepots = 24 
      # ******> TriDepot activated! The Grid has AVG Path length of: 2.8 
      # 110 edges!
      
    }else if (gridType == "sq") { 
      
      
      updateRadioButtons(
        session = session, 
        inputId = "grid",
        selected = "sq"
      )
      
      activeGridType <<- "sq"
      
      
      DTMtx <<- SqDTMtx
      depotPolygons <<- SqDepotPolygons
      depotPositions <<-  SqdepotPositions 
      
      
      nDT <<- nSqDT 
      nBT <<- SqnBT
      
      #VertexPositions <<- SqVertexPositions
      VTXMtx <<-  SqVTXMtx
      
      nBSandDepot <<- nBSandSqDepot
      nVertex <<-nSqVertex
      
      
      depotAdj <<- SqDepotAdj
      
      edgeW <<-  SqEdgeW
      D2DlinesMTX <<-  SqD2DlinesMTX
      
      DTList <<- SqDTList
      
      
      #Boat stuff
      BTList <<- SqBTList
      BTMtx <<- SqBTMtx
      BTwithCharging <<-  SqBTwithCharging
      
      BT2BTlinesMTX  <<-  SqBT2BTlinesMTX
      BT2BTMtx <<- SqBT2BTMtx
      BT2BTAdj <<- SqBT2BTAdj 
      BT2BTedgeW <<-  SqBT2BTedgeW 
      
      
      #Boat to Depot data structures
      #Active
      BT2DTlinesMTX  <<-  BT2SqDTlinesMTX
      BT2DTMtx <<- BT2SqDTMtx
      BT2DTAdj <<- BT2SqDTAdj
      BT2DTedgeW <<- BT2SqDTedgeW
      BT2DTGrayEdge <<- BT2SqDTGrayEdge
      BT2DTRedEdge <<- BT2SqDTRedEdge
      
      #Active
      gBSandDT  <<- gSqBSandDT 
      gRedGray  <<-  gSqRedGray
      gGray   <<- gSqGray
      gRedGrayYellow  <<- gSqRedGrayYellow
      
      
      #ATTENTION YOU HAVE TO USE gSqRedGray DIRECTLY NOT  VIA  gRedGray!!!!
      myGraph$value <- gSqRedGray
      
      allSPfromBS <- shortest_paths(gSqRedGray, from = 1, to = V(gSqRedGray), output = "both")
      eLen <- length(allSPfromBS$epath)
      #The first  one is to BS itself!!!
      eSum <- 0
      eCNT <- 0
      for (eIDX in (2:eLen)){
       eSum <- eSum + sum(allSPfromBS$epath[[eIDX]]$weight)
       eCNT <- eCNT + length(allSPfromBS$epath[[eIDX]])
      }
      
      avgSPFromBS <- eSum / (eLen - 1)
      avgeCNTFromBS <- eCNT / (eLen - 1)
      
      theSqAVGpathLen <<- mean_distance(gSqRedGray)
      
      dRange <- 20000
      cat("******> SqDepot activated! The Grid has AVG Path length of:",theSqAVGpathLen,"\n",
          "Drone range:", dRange,"\n",
          "From my CNT",avgeCNTFromBS,"-", avgSPFromBS, "m drone range unit:", 
          round(avgSPFromBS/dRange, 3),"\n")
      
      
      # *** Sq grid
      # BS + SqDepots =  31 BS =  1 SqDepots = 30 
      # ******> SqDepot activated! The Grid has AVG Path length of: 2.729032 
      # SqDepot activated
      # 182 edges!
      
    }
    
    
    
    
  }
  ########################################################################################################
  
  
  
  
  ########################################################################################################  
  prepareTriDepotPos <- function(){
    
    
    
    TriDTReady <<- TRUE
    TriDTMtx <<- rbind(TriDTMtx, c(BSMtx[1,1],BSMtx[1,2]))
    
    
    drange <<- input$drone_range
    
    #1.38 is kind of aspect ratio?
    TriCsize <- 3 * drange / (2 * sqrt(3)) * 1.38
    
    #sqCsize <- drange / sqrt(2) * 1.375
    rescueDist <<- floor(drange / 2)
    
    #DEBUG
    cat("****** Current drange",drange,"rescue dist",rescueDist,"Tri cell size",TriCsize,"\n")
    
    #Transform to meter axis
    x <- st_transform(st_as_sf(regionPolyDF), 3395)
    x <- st_geometry(x)
    
    #Triangular grid
    ##################################################
    TriGridPoly <- st_make_grid(x,
                                cellsize = TriCsize,
                                crs = st_crs(x),
                                square = FALSE, 
                                what = "polygons",
                                flat_topped = FALSE)
    
    
    TriGridPoly <-st_transform(TriGridPoly,  4326)
    
    
    
    ###################################################################################################
    #DEBUG
    TriGridPolyCentroids <- rgeos::gCentroid(as(TriGridPoly,"Spatial"), byid=TRUE) 
    
    ###################################################################################################
    
    
    
    #DEBUG
    cat("*** Tri grid\n")
    #Pick the points inside rescue polygon
    TriDepotPositions  <<- TriGridPolyCentroids[which(point.in.polygon(TriGridPolyCentroids$x, 
                                                                       TriGridPolyCentroids$y, 
                                                                       st_coordinates(regionPoly)[,1],  
                                                                       st_coordinates(regionPoly)[,2]) != 0)]
    
    
    
    #Pick the polygons inside rescue polygon for drawing purposes                                                     
    TriDepotPolygons <<- TriGridPoly[which(point.in.polygon(rgeos::gCentroid(as(TriGridPoly,"Spatial"), byid=TRUE)$x,
                                                            rgeos::gCentroid(as(TriGridPoly,"Spatial"), byid=TRUE)$y,
                                                            st_coordinates(regionPoly)[,1],  
                                                            st_coordinates(regionPoly)[,2]) != 0)]
    
    
    
    
    
    numTriDepots <- length(TriDepotPositions)
    nTriDT <<- nTriDT + numTriDepots
    TriDTMtx <<- rbind(TriDTMtx, cbind(TriDepotPositions$x, TriDepotPositions$y))
    
    #TriVertexPositions <<- as.data.frame(TriDepotPositions)
    TriVTXMtx <<-  TriDTMtx
    
    nBSandTriDepot <<- nBS + nTriDT
    nTriVertex <<- nBS + nTriDT
    
    ##################################################################################
    #BEGIN: Lets get the adj mtx here:
    ##################################################################################
    
    #######################################
    #BEGIN loop for among depots and BS:
    #######################################
    
    #Let find the adjacency mtx for depots + BS (index 1 is for BS) + boats
    TriDepotAdj <<- matrix(0, nrow=nBSandTriDepot, ncol=nBSandTriDepot)
    
    #Well save the distances as the "weights" for edges. Initially all are infinity
    TriEdgeW <<- matrix(Inf, nrow=nBSandTriDepot, ncol=nBSandTriDepot)
    
    #Everybody is near to itself!!!
    diag(TriEdgeW) <<- 0
    
    #DEBUG
    cat("BS + TriDepots = ",nBSandTriDepot,"BS = ",nBS, "TriDepots =",nTriDT, "\n")
    for (i in (1:(nBSandTriDepot-1))){
      for (j in ((i+1):nBSandTriDepot)){
        
        
        distD2D <- distGeo(TriDTMtx[i,], TriDTMtx[j,])
        #distD2D <- lonlat2m(DTMtx[i,1], DTMtx[i,2], DTMtx[j,1], DTMtx[j,2]) #lon1,lat1,lon2,lat2
        #DEBUG 
        #cat("Depots",i,j,"dist between",distD2D,"meters (", 
        #    round((100 * distD2D / currentDroneRange),1),
        #    ") drone range is", currentDroneRange, "-", drange ,"meters\n")
        
        if (distD2D <=  drange) {
          TriDepotAdj[i,j] <<- 1
          TriDepotAdj[j,i] <<- 1
          
          TriD2DlinesMTX <<- rbind(TriD2DlinesMTX, TriDTMtx[i,]) 
          TriD2DlinesMTX <<- rbind(TriD2DlinesMTX, TriDTMtx[j,]) 
          #DEBUG cat("Depots",i,j,"are adjacent:",distD2D,"ratio",round((distD2D / currentDroneRange),nPrecDigit),"!\n");
          #update edge weights too
          TriEdgeW[i,j] <<- distD2D
          TriEdgeW[j,i] <<- distD2D
        }
      }
    }
    
    #DEBUG
    cat("TriDepots and BSs are connected\n")
    
    #######################################
    #END loop for among depots and BS:
    #######################################
    
    
    ##################################################################################
    #END: Lets get the adj mtx here:
    ##################################################################################
    
    
    ####################################################################
    #BEGIN iPlot stuff
    ####################################################################
    
    
    #Generate the graph
    gTriBSandDT <<- graph_from_adjacency_matrix(TriDepotAdj, mode='undirected', diag=F)
    gTriRedGray <<- graph_from_adjacency_matrix(TriDepotAdj, mode='undirected', diag=F)
    gTriGray <<- graph_from_adjacency_matrix(TriDepotAdj, mode='undirected', diag=F)
    gTriRedGrayYellow <<- graph_from_adjacency_matrix(TriDepotAdj, mode='undirected', diag=F)
    
    
    #Update weights
    for (i in (1:(nBSandTriDepot-1))){
      for (j in ((i+1):(nBSandTriDepot))){
        
        if (get.edge.ids(gTriBSandDT,c(i,j)) > 0) {
          E(gTriBSandDT)[get.edge.ids(gTriBSandDT,c(i,j))]$weight <<- TriEdgeW[i,j]
          E(gTriBSandDT)[get.edge.ids(gTriBSandDT,c(j,i))]$weight <<- TriEdgeW[j,i]
        }
        
        if (get.edge.ids(gTriRedGray,c(i,j)) > 0) {
          E(gTriRedGray)[get.edge.ids(gTriRedGray,c(i,j))]$weight <<- TriEdgeW[i,j]
          E(gTriRedGray)[get.edge.ids(gTriRedGray,c(j,i))]$weight <<- TriEdgeW[j,i]
        }
        
        if (get.edge.ids(gTriGray,c(i,j)) > 0) {
          E(gTriGray)[get.edge.ids(gTriGray,c(i,j))]$weight <<- TriEdgeW[i,j]
          E(gTriGray)[get.edge.ids(gTriGray,c(j,i))]$weight <<- TriEdgeW[j,i]
        }
        
        if (get.edge.ids(gTriRedGrayYellow,c(i,j)) > 0) {
          E(gTriRedGrayYellow)[get.edge.ids(gTriRedGrayYellow,c(i,j))]$weight <<- TriEdgeW[i,j]
          E(gTriRedGrayYellow)[get.edge.ids(gTriRedGrayYellow,c(j,i))]$weight <<- TriEdgeW[j,i]
        }
        
        
      }
    }
    
    #Color the BS in red
    V(gTriBSandDT)$color[1] <<- BSCol
    V(gTriRedGray)$color[1] <<- BSCol
    V(gTriGray)$color[1] <<- BSCol
    V(gTriRedGrayYellow)$color[1] <<- BSCol
    
    #V(gRedGray)$shape[1] <<- "square"
    V(gTriBSandDT)$shape[1] <<- "myBS"
    V(gTriRedGray)$shape[1] <<- "myBS"
    V(gTriGray)$shape[1] <<- "myBS"
    V(gTriRedGrayYellow)$shape[1] <<- "myBS"
    
    V(gTriBSandDT)$shape.cex[1] <<- 0.
    V(gTriRedGray)$shape.cex[1] <<- 0.1
    V(gTriGray)$shape.cex[1] <<- 0.1
    V(gTriRedGrayYellow)$shape.cex[1] <<- 0.1
    
    
    #V(gRedGray)$shape[1] <<- 0.05
    V(gTriBSandDT)$label[1] <<- sprintf("%s", "BS1")
    V(gTriRedGray)$label[1] <<- sprintf("%s", "BS1")
    V(gTriGray)$label[1] <<- sprintf("%s", "BS1")
    V(gTriRedGrayYellow)$label[1] <<- sprintf("%s", "BS1")
    
    
    
    
    
    V(gTriBSandDT)$label.cex <<- 0.2
    V(gTriRedGray)$label.cex <<- 0.2
    V(gTriGray)$label.cex <<- 0.2
    V(gTriRedGrayYellow)$label.cex <<- 0.2
    
    for (i in (2:(nBSandTriDepot))){
      V(gTriBSandDT)$label[i] <<- i
      V(gTriRedGray)$label[i] <<- i
      V(gTriGray)$label[i] <<- i
      V(gTriRedGrayYellow)$label[i] <<- i
      
      V(gTriBSandDT)$color[i] <<- depotCol
      V(gTriRedGray)$color[i] <<- depotCol
      V(gTriGray)$color[i] <<- depotCol
      V(gTriRedGrayYellow)$color[i] <<- depotCol
      
      #V(gRedGray)$shape[i] <<- "rhombus"
      V(gTriBSandDT)$shape[i] <<- "myDT"
      V(gTriRedGray)$shape[i] <<- "myDT"
      V(gTriGray)$shape[i] <<- "myDT"
      V(gTriRedGrayYellow)$shape[i] <<- "myDT"
    }
    
    
    # for (i in (1:reac()$nBoat)){
    #   V(gRedGray)$label[i + (nDT+1)] <- sprintf("%s%d", "B",i)
    #   V(gRedGray)$color[i + (nDT+1)] <- "yellow"
    # }
    
    myCoords <- myGraphScale * TriDTMtx
    #DEBUG cat("Scale for Graph:",input$myGraphScale ,"\n")
    mylayout <- layout_nicely(gTriRedGray,dim = 2)
    
    
    par(mar=c(0,0,0,0))
    #plot(gRedGray, vertex.size=15, edge.label = E(gRedGray)$weight, layout=layout_with_graphopt(gRedGray))
    E(gTriBSandDT)$color <<- "green"
    E(gTriRedGray)$color <<- "green"
    E(gTriGray)$color <<- "green"
    E(gTriRedGrayYellow)$color <<- "green"
    
    E(gTriBSandDT)$width <<- 1
    E(gTriRedGray)$width <<- 1
    E(gTriGray)$width <<- 1
    E(gTriRedGrayYellow)$width <<- 1
    
    E(gTriBSandDT)$label.cex <<- 0.2
    E(gTriRedGray)$label.cex <<- 0.2
    E(gTriGray)$label.cex <<- 0.2
    E(gTriRedGrayYellow)$label.cex <<- 0.2
    
    
    # myGraph$value <- gTriRedGray
    
    #Maybe we can copy redGRay to others?
    #gGray <<- gRedGray
    #gRedGrayYellow <<- gRedGray
    
    #DEBUG plot(gRedGray, vertex.size=10, edge.label = E(gRedGray)$weight, layout=myCoords)
    
    
    #These lines help to keep vertex labels when you delete in between vertex.
    V(gTriBSandDT)$name <<- V(gTriBSandDT)
    V(gTriRedGray)$name <<- V(gTriRedGray)
    V(gTriGray)$name <<- V(gTriGray)
    V(gTriRedGrayYellow)$name <<-   V(gTriRedGrayYellow)
    
    ####################################################################
    #END iPlot stuff
    ####################################################################
    
    
    
    
    #BSList[[nBS]] <<- st_point(c(click$lng, click$lat))
    TriDTList <<- c(TriDTList, TriDepotPositions)
    cat("TriDepot pos list created\n")
    
    
  }
  
  
  ########################################################################################################  
  
  
  
  
  
  
  ########################################################################################################  
  prepareSqDepotPos <- function(){
    
    
    SqDTReady <<- TRUE
    
    SqDTMtx <<- rbind(SqDTMtx, c(BSMtx[1,1],BSMtx[1,2]))
    
    
    drange <<- input$drone_range
    
    #1.38 is kind of aspect ratio?
    #triCsize <- 3 * drange / (2 * sqrt(3)) * 1.38
    
    SqCsize <- drange / sqrt(2) * 1.375
    rescueDist <<- floor(drange / 2)
    
    #DEBUG
    cat("****** Current drange",drange,"rescue dist",rescueDist,"sq cell size",SqCsize,"\n")
    
    #Transform to meter axis
    x <- st_transform(st_as_sf(regionPolyDF), 3395)
    x <- st_geometry(x)
    
    #Square grid
    ##################################################
    SqGridPoly <- st_make_grid(x,
                               cellsize = SqCsize,
                               crs = st_crs(x),
                               square = TRUE,
                               what = "polygons",
                               flat_topped = FALSE)
    
    
    SqGridPoly <- st_transform(SqGridPoly,  4326)
    
    
    
    ###################################################################################################
    #DEBUG
    SqGridPolyCentroids <- rgeos::gCentroid(as(SqGridPoly,"Spatial"), byid=TRUE) 
    
    ###################################################################################################
    
    
    
    #DEBUG
    cat("*** Sq grid\n")
    #Pick the points inside rescue polygon
    SqDepotPositions  <<- SqGridPolyCentroids[which(point.in.polygon(SqGridPolyCentroids$x, 
                                                                     SqGridPolyCentroids$y, 
                                                                     st_coordinates(regionPoly)[,1],  
                                                                     st_coordinates(regionPoly)[,2]) != 0)]
    
    
    
    #Pick the polygons inside rescue polygon for drawing purposes                                                     
    SqDepotPolygons <<- SqGridPoly[which(point.in.polygon(rgeos::gCentroid(as(SqGridPoly,"Spatial"), byid=TRUE)$x,
                                                          rgeos::gCentroid(as(SqGridPoly,"Spatial"), byid=TRUE)$y,
                                                          st_coordinates(regionPoly)[,1],  
                                                          st_coordinates(regionPoly)[,2]) != 0)]
    
    
    
    
    
    numSqDepots <- length(SqDepotPositions)
    nSqDT <<- nSqDT + numSqDepots
    SqDTMtx <<- rbind(SqDTMtx, cbind(SqDepotPositions$x, SqDepotPositions$y))
    
    #SqVertexPositions <<- as.data.frame(SqDepotPositions)
    SqVTXMtx <<-  SqDTMtx
    
    nBSandSqDepot <<- nBS + nSqDT
    nSqVertex <<- nBS + nSqDT
    
    ##################################################################################
    #BEGIN: Lets get the adj mtx here:
    ##################################################################################
    
    #######################################
    #BEGIN loop for among depots and BS:
    #######################################
    
    #Let find the adjacency mtx for depots + BS (index 1 is for BS) + boats
    SqDepotAdj <<- matrix(0, nrow=nBSandSqDepot, ncol=nBSandSqDepot)
    
    #Well save the distances as the "weights" for edges. Initially all are infinity
    SqEdgeW <<- matrix(Inf, nrow=nBSandSqDepot, ncol=nBSandSqDepot)
    
    #Everybody is near to itself!!!
    diag(SqEdgeW) <<- 0
    
    #DEBUG
    cat("BS + SqDepots = ",nBSandSqDepot,"BS = ",nBS, "SqDepots =",nSqDT, "\n")
    for (i in (1:(nBSandSqDepot-1))){
      for (j in ((i+1):nBSandSqDepot)){
        
        
        distD2D <- distGeo(SqDTMtx[i,], SqDTMtx[j,])
        #distD2D <- lonlat2m(DTMtx[i,1], DTMtx[i,2], DTMtx[j,1], DTMtx[j,2]) #lon1,lat1,lon2,lat2
        #DEBUG 
        #cat("Depots",i,j,"dist between",distD2D,"meters (", 
        #    round((100 * distD2D / currentDroneRange),1),
        #    ") drone range is", currentDroneRange, "-", drange ,"meters\n")
        
        if (distD2D <=  drange) {
          SqDepotAdj[i,j] <<- 1
          SqDepotAdj[j,i] <<- 1
          
          SqD2DlinesMTX <<- rbind(SqD2DlinesMTX, SqDTMtx[i,]) 
          SqD2DlinesMTX <<- rbind(SqD2DlinesMTX, SqDTMtx[j,]) 
          #DEBUG cat("Depots",i,j,"are adjacent:",distD2D,"ratio",round((distD2D / currentDroneRange),nPrecDigit),"!\n");
          #update edge weights too
          SqEdgeW[i,j] <<- distD2D
          SqEdgeW[j,i] <<- distD2D
        }
      }
    }
    
    #DEBUG
    cat("SqDepots and BSs are connected\n")
    
    #######################################
    #END loop for among depots and BS:
    #######################################
    
    
    ##################################################################################
    #END: Lets get the adj mtx here:
    ##################################################################################
    
    
    ####################################################################
    #BEGIN iPlot stuff
    ####################################################################
    
    
    #Generate the graph
    gSqBSandDT <<- graph_from_adjacency_matrix(SqDepotAdj, mode='undirected', diag=F)
    gSqRedGray <<- graph_from_adjacency_matrix(SqDepotAdj, mode='undirected', diag=F)
    gSqGray <<- graph_from_adjacency_matrix(SqDepotAdj, mode='undirected', diag=F)
    gSqRedGrayYellow <<- graph_from_adjacency_matrix(SqDepotAdj, mode='undirected', diag=F)
    
    
    #Update weights
    for (i in (1:(nBSandSqDepot-1))){
      for (j in ((i+1):(nBSandSqDepot))){
        
        if (get.edge.ids(gSqBSandDT,c(i,j)) > 0) {
          E(gSqBSandDT)[get.edge.ids(gSqBSandDT,c(i,j))]$weight <<- SqEdgeW[i,j]
          E(gSqBSandDT)[get.edge.ids(gSqBSandDT,c(j,i))]$weight <<- SqEdgeW[j,i]
        }
        
        if (get.edge.ids(gSqRedGray,c(i,j)) > 0) {
          E(gSqRedGray)[get.edge.ids(gSqRedGray,c(i,j))]$weight <<- SqEdgeW[i,j]
          E(gSqRedGray)[get.edge.ids(gSqRedGray,c(j,i))]$weight <<- SqEdgeW[j,i]
        }
        
        if (get.edge.ids(gSqGray,c(i,j)) > 0) {
          E(gSqGray)[get.edge.ids(gSqGray,c(i,j))]$weight <<- SqEdgeW[i,j]
          E(gSqGray)[get.edge.ids(gSqGray,c(j,i))]$weight <<- SqEdgeW[j,i]
        }
        
        if (get.edge.ids(gSqRedGrayYellow,c(i,j)) > 0) {
          E(gSqRedGrayYellow)[get.edge.ids(gSqRedGrayYellow,c(i,j))]$weight <<- SqEdgeW[i,j]
          E(gSqRedGrayYellow)[get.edge.ids(gSqRedGrayYellow,c(j,i))]$weight <<- SqEdgeW[j,i]
        }
        
        
      }
    }
    
    #Color the BS in red
    V(gSqBSandDT)$color[1] <<- BSCol
    V(gSqRedGray)$color[1] <<- BSCol
    V(gSqGray)$color[1] <<- BSCol
    V(gSqRedGrayYellow)$color[1] <<- BSCol
    
    #V(gRedGray)$shape[1] <<- "square"
    V(gSqBSandDT)$shape[1] <<- "myBS"
    V(gSqRedGray)$shape[1] <<- "myBS"
    V(gSqGray)$shape[1] <<- "myBS"
    V(gSqRedGrayYellow)$shape[1] <<- "myBS"
    
    V(gSqBSandDT)$shape.cex[1] <<- 0.
    V(gSqRedGray)$shape.cex[1] <<- 0.1
    V(gSqGray)$shape.cex[1] <<- 0.1
    V(gSqRedGrayYellow)$shape.cex[1] <<- 0.1
    
    
    #V(gRedGray)$shape[1] <<- 0.05
    V(gSqBSandDT)$label[1] <<- sprintf("%s", "BS1")
    V(gSqRedGray)$label[1] <<- sprintf("%s", "BS1")
    V(gSqGray)$label[1] <<- sprintf("%s", "BS1")
    V(gSqRedGrayYellow)$label[1] <<- sprintf("%s", "BS1")
    
    V(gSqBSandDT)$label.cex <<- 0.2
    V(gSqRedGray)$label.cex <<- 0.2
    V(gSqGray)$label.cex <<- 0.2
    V(gSqRedGrayYellow)$label.cex <<- 0.2
    
    for (i in (2:(nBSandSqDepot))){
      V(gSqBSandDT)$label[i] <<- i
      V(gSqRedGray)$label[i] <<- i
      V(gSqGray)$label[i] <<- i
      V(gSqRedGrayYellow)$label[i] <<- i
      
      V(gSqBSandDT)$color[i] <<- depotCol
      V(gSqRedGray)$color[i] <<- depotCol
      V(gSqGray)$color[i] <<- depotCol
      V(gSqRedGrayYellow)$color[i] <<- depotCol
      
      #V(gRedGray)$shape[i] <<- "rhombus"
      V(gSqBSandDT)$shape[i] <<- "myDT"
      V(gSqRedGray)$shape[i] <<- "myDT"
      V(gSqGray)$shape[i] <<- "myDT"
      V(gSqRedGrayYellow)$shape[i] <<- "myDT"
    }
    
    
    # for (i in (1:reac()$nBoat)){
    #   V(gRedGray)$label[i + (nDT+1)] <- sprintf("%s%d", "B",i)
    #   V(gRedGray)$color[i + (nDT+1)] <- "yellow"
    # }
    
    myCoords <- myGraphScale * SqDTMtx
    #DEBUG cat("Scale for Graph:",input$myGraphScale ,"\n")
    mylayout <- layout_nicely(gSqRedGray,dim = 2)
    
    
    par(mar=c(0,0,0,0))
    #plot(gRedGray, vertex.size=15, edge.label = E(gRedGray)$weight, layout=layout_with_graphopt(gRedGray))
    E(gSqBSandDT)$color <<- "green"
    E(gSqRedGray)$color <<- "green"
    E(gSqGray)$color <<- "green"
    E(gSqRedGrayYellow)$color <<- "green"
    
    E(gSqBSandDT)$width <<- 1
    E(gSqRedGray)$width <<- 1
    E(gSqGray)$width <<- 1
    E(gSqRedGrayYellow)$width <<- 1
    
    E(gSqBSandDT)$label.cex <<- 0.2
    E(gSqRedGray)$label.cex <<- 0.2
    E(gSqGray)$label.cex <<- 0.2
    E(gSqRedGrayYellow)$label.cex <<- 0.2
    
    
    #myGraph$value <- gSqRedGray
    #cat("Just after setting the value for myGraph\n")
    
    #Maybe we can copy redGRay to others?
    #gGray <<- gRedGray
    #gRedGrayYellow <<- gRedGray
    
    #DEBUG plot(gRedGray, vertex.size=10, edge.label = E(gRedGray)$weight, layout=myCoords)
    
    #These lines help to keep vertex labels when you delete in between vertex.
    V(gSqBSandDT)$name <<- V(gSqBSandDT)
    V(gSqRedGray)$name <<- V(gSqRedGray)
    V(gSqGray)$name <<- V(gSqGray)
    V(gSqRedGrayYellow)$name <<-   V(gSqRedGrayYellow)
    
    ####################################################################
    #END iPlot stuff
    ####################################################################
    
    
    
    
    #BSList[[nBS]] <<- st_point(c(click$lng, click$lat))
    SqDTList <<- c(SqDTList, SqDepotPositions)
    cat("SqDepot pos list created\n")
    
    
  }
  
  
  ######################################################################################################## 
  
  
  
  
  ########################################################################################################
  
  drawTriDepots <- function(){
    ################################################
    isolate({
      new_zoom <- 9
      if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
      
      leafletProxy("mymap") %>% clearGroup("depotPolys")  %>% clearGroup("depotCircles")
      
      leafletProxy("mymap") %>% 
        addMarkers(data = as.data.frame(TriDTMtx),
                   lng = ~ V1, 
                   lat = ~ V2,
                   icon = myDTIcon)  
      
      mapToSave$current <- mapToSave$base %>% 
        addMarkers(data = as.data.frame(TriDTMtx),
                   lng = ~ V1, 
                   lat = ~ V2,
                   icon = myDTIcon)  
      
      
      
      ####################################################################      
      # We need lines between depots
      ####################################################################
      
      Nlines <- length(TriD2DlinesMTX) / 2
      #DEBUG
      cat (Nlines, "edges!\n")
      
      for (k in seq(1,Nlines-1,by = 2)) {
        
        df <- as.data.frame(rbind(TriD2DlinesMTX[k,], TriD2DlinesMTX[k+1,]))
        
        leafletProxy("mymap") %>% 
          addPolylines(data = df,
                       lng = ~ V1, 
                       lat = ~ V2,
                       color = "green",
                       weight = 2)
        
        mapToSave$current <- mapToSave$base %>% 
          addPolylines(data = df,
                       lng = ~ V1, 
                       lat = ~ V2,
                       color = "green",
                       weight = 2)
        
      }
      
      ####################################################################     
      #DEBUG
      cat (Nlines, "edges drawn!\n")
      #############################################
      
      # leafletProxy("mymap") %>%
      #   addPolygons(data = TridepotPolygons,
      #               color ="red",
      #               weight = 0.5,
      #               opacity = 5,
      #               group = "TriDepotPolys") %>%
      #   
      #   #Draw circles for the range of depots and check the aspect ratio
      #   addCircles(data = as.data.frame(TriDTMtx), 
      #              lat = ~ V2, 
      #              lng = ~ V1, 
      #              weight = 1, 
      #              radius = rescueDist,
      #              #popup = as.character("Depot"),
      #              #label = as.character(paste0("Depot: ")),
      #              color = "blue", 
      #              fillOpacity = 0.2,
      #              group = "TriDepotCircles")
      # 
      
      cat("TriDepots added to map\n")
    })#isolate
    ################################################
    
  }
  
  ########################################################################################################
  
  
  
  
  ########################################################################################################
  
  drawSqDepots <- function(){
    ################################################
    isolate({
      new_zoom <- 9
      if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
      
      leafletProxy("mymap") %>% clearGroup("depotPolys")  %>% clearGroup("depotCircles")
      mapToSave$current <- mapToSave$base %>% clearGroup("depotPolys")  %>% clearGroup("depotCircles")
      
      
      leafletProxy("mymap") %>% 
        addMarkers(data = as.data.frame(SqDTMtx),
                   lng = ~ V1, 
                   lat = ~ V2,
                   icon = myDTIcon)  
      
      mapToSave$current <- mapToSave$base %>% 
        addMarkers(data = as.data.frame(SqDTMtx),
                   lng = ~ V1, 
                   lat = ~ V2,
                   icon = myDTIcon)  
      
      
      
      ####################################################################      
      # We need lines between depots
      ####################################################################
      
      Nlines <- length(SqD2DlinesMTX) / 2
      #DEBUG
      cat (Nlines, "edges!\n")
      
      for (k in seq(1,Nlines-1,by = 2)) {
        
        df <- as.data.frame(rbind(SqD2DlinesMTX[k,], SqD2DlinesMTX[k+1,]))
        
        leafletProxy("mymap") %>% 
          addPolylines(data = df,
                       lng = ~ V1, 
                       lat = ~ V2,
                       color = "green",
                       weight = 2)
        
        mapToSave$current <- mapToSave$base %>% 
          addPolylines(data = df,
                       lng = ~ V1, 
                       lat = ~ V2,
                       color = "green",
                       weight = 2)
        
      }
      
      ####################################################################     
      #DEBUG
      cat (Nlines, "edges drawn!\n")
      #############################################
      
      # leafletProxy("mymap") %>%
      #   addPolygons(data = SqDepotPolygons,
      #               color ="red",
      #               weight = 0.5,
      #               opacity = 5,
      #               group = "SqDepotPolys") %>%
      #   
      #   #Draw circles for the range of depots and check the aspect ratio
      #   addCircles(data = as.data.frame(SqDTMtx), 
      #              lat = ~ V2, 
      #              lng = ~ V1, 
      #              weight = 1, 
      #              radius = rescueDist,
      #              #popup = as.character("Depot"),
      #              #label = as.character(paste0("Depot: ")),
      #              color = "blue", 
      #              fillOpacity = 0.2,
      #              group = "SqDepotCircles")
      
      
      cat("SqDepots added to map\n")
    })#isolate
    ################################################
    
  }
  
  ########################################################################################################
  
  
  
  
  ########################################################################################################
  
  drawActiveDepots <- function(){
    ################################################
    isolate({
      new_zoom <- 9
      if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
      
      #Clear the previous stuff if any
      
      leafletProxy("mymap") %>%  clearMarkers() %>% clearGroup("depotPolys") %>% clearGroup("depotCircles")  %>% 
        clearGroup("depotEdges") %>% clearGroup("BT2DTEdges") %>% clearGroup("boatCircles")
      
      mapToSave$current <- mapToSave$base %>%  clearMarkers() %>% clearGroup("depotPolys") %>% 
        clearGroup("depotCircles")  %>% clearGroup("depotEdges") %>% clearGroup("BT2DTEdges") %>% clearGroup("boatCircles")
      
      
      
      
      leafletProxy("mymap") %>% 
        addMarkers(data = as.data.frame(BSMtx),
                   lng = ~ V1, 
                   lat = ~ V2,
                   icon = myBSIcon)  
      
      # %>% 
      #   addPolylines(data = as.data.frame(BSMtx),
      #                lng = ~ V1, lat = ~ V2,
      #                color = "green",
      #                weight = 3)
      
      
      
      
      mapToSave$current <- mapToSave$base %>% 
        addMarkers(data = as.data.frame(BSMtx),
                   lng = ~ V1, 
                   lat = ~ V2,
                   icon = myBSIcon)  
      
      # %>% 
      #   addPolylines(data = as.data.frame(BSMtx),
      #                lng = ~ V1, lat = ~ V2,
      #                color = "green",
      #                weight = 3)
      
      
      leafletProxy("mymap") %>% 
        addMarkers(data = as.data.frame(DTMtx),
                   lng = ~ V1, 
                   lat = ~ V2,
                   icon = myDTIcon)  
      
      mapToSave$current <- mapToSave$base %>% 
        addMarkers(data = as.data.frame(DTMtx),
                   lng = ~ V1, 
                   lat = ~ V2,
                   icon = myDTIcon)  
      
      
      
      ####################################################################      
      # We need lines between depots
      ####################################################################
      
      Nlines <- length(D2DlinesMTX) / 2
      #DEBUG
      cat (Nlines, "edges!\n")
      
      for (k in seq(1,Nlines-1,by = 2)) {
        
        df <- as.data.frame(rbind(D2DlinesMTX[k,], D2DlinesMTX[k+1,]))
        
        leafletProxy("mymap") %>% 
          addPolylines(data = df,
                       lng = ~ V1, 
                       lat = ~ V2,
                       color = "green",
                       weight = 3,
                       group = "depotEdges")
        
        mapToSave$current <- mapToSave$base %>% 
          addPolylines(data = df,
                       lng = ~ V1, 
                       lat = ~ V2,
                       color = "green",
                       weight = 3,
                       group = "depotEdges")
        
      }
      
      ####################################################################     
      #DEBUG
      cat (Nlines, "edges drawn!\n")
      #############################################
      
      #If coverage is on put range circles too
      if (input$cov){
        leafletProxy("mymap") %>%
          addPolygons(data = depotPolygons,
                      color ="red",
                      weight = 0.5,
                      opacity = 3,
                      group = "depotPolys") %>%
          
          #Draw circles for the range of depots and check the aspect ratio
          addCircles(data = as.data.frame(DTMtx), 
                     lat = ~ V2, 
                     lng = ~ V1, 
                     weight = 1, 
                     radius = rescueDist,
                     #popup = as.character("Depot"),
                     #label = as.character(paste0("Depot: ")),
                     color = "blue", 
                     fillOpacity = 0.1,
                     group = "depotCircles")
        
        
        
        mapToSave$current <- mapToSave$base %>% 
          addPolygons(data = depotPolygons,
                      color ="red",
                      weight = 0.5,
                      opacity = 3,
                      group = "depotPolys") %>%
          
          #Draw circles for the range of depots and check the aspect ratio
          addCircles(data = as.data.frame(DTMtx), 
                     lat = ~ V2, 
                     lng = ~ V1, 
                     weight = 1, 
                     radius = rescueDist,
                     #popup = as.character("Depot"),
                     #label = as.character(paste0("Depot: ")),
                     color = "blue", 
                     fillOpacity = 0.1,
                     group = "depotCircles")
        
      }
      
      
      
      
      
      cat("Active Depots added to map\n")
    })#isolate
    ################################################
    
  }
  
  ########################################################################################################
  
  
  
  ########################################################################################################
  observeEvent(input$deployTriDepots,{
    
    cat("Deploying TriDepots\n")
    
    setButtonsAfterDeployDepots()
    
    disable("deployTriDepots")
    enable("deploySqDepots")
    
    #Prepare depot positions and draw them on the map
    if (TriDTReady == FALSE) prepareTriDepotPos()
    if (SqDTReady == FALSE) prepareSqDepotPos()
    activateDepots("tri")
    drawActiveDepots()
    cat("TriDepots deployed.\n")
  })
  ########################################################################################################
  
  
  
  ########################################################################################################
  observeEvent(input$deploySqDepots,{
    
    cat("Deploying SqDepots\n")
    setButtonsAfterDeployDepots()
    
    enable("deployTriDepots")
    disable("deploySqDepots")
    
    #Prepare depot positions and draw them on the map
    if (SqDTReady == FALSE) prepareSqDepotPos()
    if (TriDTReady == FALSE) prepareTriDepotPos()
    activateDepots("sq")
    drawActiveDepots()
    cat("Sq Depots deployed.\n")
  })
  ########################################################################################################
  
  
  
  
  
  
  ######################################################################################################## 
  resetForNewBoatSet <- function() {
    #########################################
    #Reset Boats
    #########################################
    #Boolean for marking the begin of selecting Boat points
    #Initially the bs selection is open
    
    cat("************************ Resetting boats! **************************\n")
    
    #Save the number of boats
    numBT <- nBT
    
    btBEGIN <<- TRUE
    nBT <<- 0 
    BTList <<- list()
    BTMtx <<- NULL
    
    #Boats that can charge the drone
    #Assign it randomly?
    BTwithCharging <<- list()
    
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
    
    
    #Total vertices for the graph. BSs + Depots + Boats
    #Active
    nVertex <<- nBS + nDT
    #vertexPositions <<- NULL
    VTXMtx <<- VTXMtx[1:nVertex,]
    
    #Tri
    TrinBT <<- 0
    TriBTMtx <<- NULL
    nTriVertex <<- nBS + nTriDT
    #TrivertexPositions <<- NULL
    TriVTXMtx <<- TriVTXMtx[1:nTriVertex,]
    
    #Sq
    SqnBT <<- 0
    SqBTMtx <<- NULL
    nSqVertex <<- nBS + nSqDT
    #SqvertexPositions <<- NULL
    SqVTXMtx <<-  SqVTXMtx[1:nSqVertex,]
    
    
    #########################################
    
    
    
    ####################################################################################
    #The graph stuff
    ####################################################################################
    
    #Remove the last nBT vertices from these graphs 
    
    
    cat("********** Removing",numBT,"boats!\n")
    
    for (vno in 1:numBT) {
      #Active
      gRedGray <<- gRedGray %>% delete_vertices(length(V(gRedGray)))
      gGray  <<- gGray %>% delete_vertices(length(V(gGray)))
      gRedGrayYellow  <<- gRedGrayYellow %>% delete_vertices(length(V(gRedGrayYellow)))
      
      #Tri
      gTriRedGray <<- gTriRedGray %>% delete_vertices(length(V(gTriRedGray)))
      gTriGray  <<- gTriGray %>% delete_vertices(length(V(gTriGray)))
      gTriRedGrayYellow  <<- gTriRedGrayYellow %>% delete_vertices(length(V(gTriRedGrayYellow)))
      
      #Sq
      gSqRedGray <<- gSqRedGray %>% delete_vertices(length(V(gSqRedGray)))
      gSqGray  <<- gSqGray %>% delete_vertices(length(V(gSqGray)))
      gSqRedGrayYellow  <<- gSqRedGrayYellow %>% delete_vertices(length(V(gSqRedGrayYellow)))
    }
    
    
    cat("Resetting boats done!\n")
    #########################################
    #Reset Boats
    #########################################
  }
  ########################################################################################################
  
  
  
  
  
  
  
  
  
  ########################################################################################################
  #This functions checks reachability for the depot+bs only
  #Still BT 2 BT not included!!!
  checkIfAllBoatsReachableForGrids <- function(boatPos){
    
    nBoats <- nrow(boatPos)
    allReachable <- TRUE
    
    rescueDist <<- floor(input$drone_range / 2) 
    cat("*******************************************\n")
    
    #Checking for Tri
    TriBoatReachabilityVec <- rep(0, nBoats)
    TriAllReachable <- TRUE
    
    #Checking for Sq
    SqBoatReachabilityVec <- rep(0, nBoats)
    SqAllReachable <- TRUE
    
    
    for (b in 1:nBoats) {
      btLng <- boatPos[b,]$x
      btLat <- boatPos[b,]$y
      
      cat("Checking boats for Tri", nTriVertex,"BS+DT\n")
      
      for (v in 1:nBSandTriDepot) {
        dist2Boat <- lonlat2m(btLng,  btLat, TriDTMtx[v,1], TriDTMtx[v,2]) #lon1,lat1,lon2,lat2
        if (dist2Boat <= rescueDist) {
          TriBoatReachabilityVec[b] <- TriBoatReachabilityVec[b] + 1
        }
      }
      
      
      
      
      TriReachability <- TriBoatReachabilityVec[b]
      if (TriReachability == 0) {
        cat("Boat",b,"can not be reached in TriGrid!\n")
        TriAllReachable <- FALSE
        allReachable <- FALSE
        break
      }
      
      
      cat("Checking boats for Sq", nSqVertex,"BS+DT\n")
      
      for (v in 1:nBSandSqDepot) {
        dist2Boat <- lonlat2m(btLng,  btLat, SqDTMtx[v,1], SqDTMtx[v,2]) #lon1,lat1,lon2,lat2
        if (dist2Boat <= rescueDist) {
          SqBoatReachabilityVec[b] <- SqBoatReachabilityVec[b] + 1
        }
      }
      
      SqReachability <- SqBoatReachabilityVec[b]
      if (SqReachability == 0) {
        cat("Boat",b,"can not be reached in SqGrid!\n")
        SqAllReachable <- FALSE
        allReachable <- FALSE
        break
      }
      
      
    }
    
    
    
    
    
    
    
    cat("*******************************************\n")
    return(allReachable)
  }
  
  ########################################################################################################
  
  
  
  
  
  ######################################################################################################## 
  #Adds single boat to Tri Grid!!!
  #Make sure before calling this function that the boat is reachable for all Grid types!!!
  addBoatTri <- function(boatPos, btIDX){
    
    
    
    currentDroneRange <- input$drone_range
    btLng <- boatPos[btIDX,]$x
    btLat <- boatPos[btIDX,]$y
    
    
    TrinBT <<- TrinBT + 1
    nTriVertex <<- nBS + nTriDT + TrinBT
    
    #DEBUG
    cat("Adding boat", btIDX,"at", btLng, btLat," to Tri Grid.\n")
    cat("Drone range",currentDroneRange," meters. In polygon Vertex no:",nTriVertex,"Boat No:",TrinBT,"\n")
    
    #LATER nBSandTriDepot  <<- nBSandTriDepot + 1
    
    
    
    text2 <- paste0("BT point[", TrinBT,"] ", btIDX, "at: ", btLng , btLat)
    
    cat("Tri Total Boats:", TrinBT,"\n")
    cat("Tri --- Before adding Boat   n(TriBTMtx):", nrow(TriBTMtx), "n(TriVTXMtx):", nrow(TriVTXMtx),"\n")
    TriBTMtx <<- rbind(TriBTMtx, c(btLng, btLat))
    TriVTXMtx <<- rbind(TriVTXMtx, c(btLng, btLat))
    cat("Tri --- After adding Boat   n(TriBTMtx):", nrow(TriBTMtx), "n(TriVTXMtx):", nrow(TriVTXMtx),"\n")
    
    #vertexPositions <<- rbind(vertexPositions, c(btLng, btLat))
    
    
    
    
    ###########################################################################
    #Here lets add it to graph!!!
    #Add the boat to BT2TriDTMtx first as a row
    
    ###########################################################################
    #We add the boats to the graph labeling them after nBS+nDT (BSs + depots)
    #But for them the adjacency condition is that they are at the 
    #rescueDist from some depots.
    ###########################################################################
    
    
    
    
    ###########################################################################
    #Init and modify some data structures of Boats and Depots
    ###########################################################################
    #Let find the adjacency mtx for depots + BS (index 1 is for BS) + boats
    BT2TriDTAdj <<- rbind(BT2TriDTAdj, matrix(0, nrow=1, ncol=nBSandTriDepot))
    #Adj for gray edges
    BT2TriDTGrayEdge <<- rbind(BT2TriDTGrayEdge, matrix(0, nrow=1, ncol=nBSandTriDepot))
    #Adj for red edges
    BT2TriDTRedEdge <<- rbind(BT2TriDTRedEdge, matrix(0, nrow=1, ncol=nBSandTriDepot))
    #These mtx s have same size!!!
    
    
    nr <- dim(TriBT2BTAdj)[1]
    if (is.null(nr)) { nr <- 1} 
    #Add the last column and then the last row to "expand" the adj matrix
    TriBT2BTAdj <<- cbind(TriBT2BTAdj, matrix(0, nrow=nr, ncol=1))
    TriBT2BTAdj <<- rbind(TriBT2BTAdj, matrix(0, nrow=1, ncol=TrinBT))
    
    
    
    
    #Well save the distances as the "weights" for edges. Initially all are infinity
    BT2TriDTedgeW <<- rbind(BT2TriDTedgeW, matrix(Inf, nrow=1, ncol=nBSandTriDepot))
    
    
    nr <- dim(TriBT2BTedgeW)[1]
    if (is.null(nr)) { nr <- 1} 
    TriBT2BTedgeW <<- cbind(TriBT2BTedgeW, matrix(Inf, nrow=nr, ncol=1))
    TriBT2BTedgeW <<- rbind(TriBT2BTedgeW, matrix(Inf, nrow=1, ncol=TrinBT))
    
    
    
    #Add the row for that Boat and the vertex to the graph
    BT2TriDTMtx <<- rbind(BT2TriDTMtx, matrix(Inf, nrow = 1, ncol = nBSandTriDepot))
    
    
    nr <- dim(TriBT2BTMtx)[1]
    if (is.null(nr)) { nr <- 1} 
    TriBT2BTMtx <<- cbind(TriBT2BTMtx, matrix(Inf, nrow = nr, ncol = 1))
    TriBT2BTMtx <<- rbind(TriBT2BTMtx, matrix(Inf, nrow = 1, ncol = TrinBT))
    
    #DEBUG
    cat("Adding vertex:", nTriVertex, "with label:", paste0("B",TrinBT),"\n")
    gTriRedGray <<- gTriRedGray             %>% add_vertices(1, color="lightblue", shape = "myBT", name = nTriVertex, label = paste0("B",TrinBT), label.cex = 0.2)
    gTriRedGrayYellow <<- gTriRedGrayYellow %>% add_vertices(1, color="lightblue", shape = "myBT", name = nTriVertex, label = paste0("B",TrinBT), label.cex = 0.2)
    gTriGray <<- gTriGray                   %>% add_vertices(1, color="lightblue", shape = "myBT", name = nTriVertex, label = paste0("B",TrinBT), label.cex = 0.2)
    
    
    # if(input$edgeFilter=="gr"){
    #   myGraph$value <- gTriGray
    # }else if(input$edgeFilter=="rg"){
    #   myGraph$value <- gTriRedGray
    # }else if(input$edgeFilter=="rgy"){
    #   myGraph$value <- gTriRedGrayYellow
    # }
    
    
    #Buffer the lines for the current boat and plot them at the end.
    BT2TriDTlinesBuffer <- NULL
    TriBT2BTlinesBuffer <- NULL
    
    
    ########################################
    #BEGIN loops for between depots and boats
    ########################################  
    
    ########################################
    #First add gray edges
    #Check the distance to all depots
    ########################################
    
    rescueDist <<- floor(currentDroneRange / 2) # + rangeDelta
    
    
    for (i in (1:nBSandTriDepot)){
      
      #Find the dist in meters between the boats and the depots
      BT2TriDTMtx[TrinBT, i] <<- lonlat2m(TriDTMtx[i,1], TriDTMtx[i,2], TriBTMtx[TrinBT,1], TriBTMtx[TrinBT,2]) #lon1,lat1,lon2,lat2
      
      
      #DEBUG
      #cat("**** Boat:", TrinBT,"depot:" ,i,"coords",TriDTMtx[i,],"\n")
      
      #DEBUG if (i==1) cat("Dist between ",i, "at (",dx1,dy1,") and",j, "is",dist,"\n")
      #DEBUG cat ("Dist = ",dist,"\n")
      
      #Connect the boat to depot that can be reached
      
      if (BT2TriDTMtx[TrinBT, i] <= rescueDist) {
        ####################################
        #GRAY edges here
        ####################################
        #BT2TriDTAdj[i,j] <- 1
        BT2TriDTAdj[TrinBT,i] <<- 1
        
        BT2TriDTGrayEdge[TrinBT,i] <<- 1
        
        
        BT2TriDTlinesMTX <<- rbind(BT2TriDTlinesMTX, TriDTMtx[i,]) 
        BT2TriDTlinesMTX <<- rbind(BT2TriDTlinesMTX, TriBTMtx[TrinBT,])
        
        BT2TriDTlinesBuffer <- rbind(BT2TriDTlinesBuffer, c(TriDTMtx[i,], "gray")) 
        BT2TriDTlinesBuffer <- rbind(BT2TriDTlinesBuffer, c(TriBTMtx[TrinBT,], "gray"))
        
        
        #update edge weights too
        #BT2TriDTedgeW[i,j] <- BT2TriDTMtx[i,j]
        BT2TriDTedgeW[TrinBT,i] <<- BT2TriDTMtx[TrinBT,i]
        
        
        
        #DEBUG
        cat("GRAY Reachable: Connecting vertex", nTriVertex, "Boat no", TrinBT ,"to depot", i,"dist=",BT2TriDTedgeW[TrinBT,i],"\n")
        gTriRedGray <<- gTriRedGray             %>% add_edges(c(nTriVertex,i), color=grayEdgeCol, width = 1, weight = BT2TriDTedgeW[TrinBT,i])
        gTriRedGrayYellow <<- gTriRedGrayYellow %>% add_edges(c(nTriVertex,i), color=grayEdgeCol, width = 1, weight = BT2TriDTedgeW[TrinBT,i])
        gTriGray <<- gTriGray                   %>% add_edges(c(nTriVertex,i), color=grayEdgeCol, width = 1, weight = BT2TriDTedgeW[TrinBT,i])
        
        # if(input$edgeFilter=="gr"){
        #   myGraph$value <- gTriGray
        # }else if(input$edgeFilter=="rg"){
        #   myGraph$value <- gTriRedGray
        # }else if(input$edgeFilter=="rgy"){
        #   myGraph$value <- gTriRedGrayYellow
        # }
      } #if for gray edge
    } #end of for loop over the depots
    
    
    ########################################
    #Then red edges
    #Check the distance to all depots
    ########################################
    
    for (i in (1:nBSandTriDepot)){
      
      
      #TODO
      #Here you need an vector that shows you if the boat has gray edge
      
      nGrayEdge <- sum(BT2TriDTGrayEdge[TrinBT,])
      
      if (nGrayEdge >= 1) {
        minGrayEdgeWeight <- min(BT2TriDTedgeW[TrinBT,which(BT2TriDTGrayEdge[TrinBT,] == 1)])
        minGrayEdgeSrcVtxId <- which(BT2TriDTedgeW[TrinBT,] == minGrayEdgeWeight)
        
        
        if ( (BT2TriDTRedEdge[TrinBT, i] == 0) && 
             (BT2TriDTGrayEdge[TrinBT, i] == 0) && 
             ((BT2TriDTMtx[TrinBT,i] + minGrayEdgeWeight) <= currentDroneRange) ) {
          
          ################################################################
          #RED edges here
          ################################################################
          
          BT2TriDTAdj[TrinBT,i] <<- 1
          BT2TriDTRedEdge[TrinBT,i] <<- 1
          
          BT2TriDTlinesMTX <<- rbind(BT2TriDTlinesMTX, TriDTMtx[i,]) 
          BT2TriDTlinesMTX <<- rbind(BT2TriDTlinesMTX, TriBTMtx[TrinBT,])
          
          BT2TriDTlinesBuffer <- rbind(BT2TriDTlinesBuffer, c(TriDTMtx[i,],"red")) 
          BT2TriDTlinesBuffer <- rbind(BT2TriDTlinesBuffer, c(TriBTMtx[TrinBT,],"red"))
          
          
          #update edge weights too
          #BT2TriDTedgeW[i,j] <- BT2TriDTMtx[i,j]
          BT2TriDTedgeW[TrinBT,i] <<- BT2TriDTMtx[TrinBT,i]
          
          
          
          #DEBUG
          cat("RED Reachable: Connecting vertex", nTriVertex, "Boat no", TrinBT ,"to depot", i,"dist=",BT2TriDTedgeW[TrinBT,i],"\n")
          gTriRedGray <<- gTriRedGray             %>% add_edges(c(nTriVertex,i), color=redEdgeCol, width = 1, weight = BT2TriDTedgeW[TrinBT,i])
          gTriRedGrayYellow <<- gTriRedGrayYellow %>% add_edges(c(nTriVertex,i), color=redEdgeCol, width = 1, weight = BT2TriDTedgeW[TrinBT,i])
          
          # if(input$edgeFilter=="gr"){
          #   myGraph$value <- gTriGray
          # }else if(input$edgeFilter=="rg"){
          #   myGraph$value <- gTriRedGray
          # }else if(input$edgeFilter=="rgy"){
          #   myGraph$value <- gTriRedGrayYellow
          # }
          
        }# if for red edge
        
      }else{
        
        cat("No GRAY edge for", nTriVertex, "Boat no", TrinBT,"\n")
      }# else no gray edge
      
    } #end of for loop over the depots
    
    
    
    
    
    
    
    
    
    
    
    #######################################         
    #END loops for between depots and boats
    #######################################   
    
    
    
    
    ######################################################### 
    #BEGIN loop for among boats. IF YOU HAVE AT LEAST 2 BOATS
    ######################################################### 
    
    #Check the current boat "TrinBT" against all the others
    if (TrinBT >= 2) {
      for (i in (1:(TrinBT-1))){
        
        #Find the dist in meters between two boats
        TriBT2BTMtx[TrinBT, i] <<- lonlat2m(TriBTMtx[i,1], TriBTMtx[i,2], TriBTMtx[TrinBT,1], TriBTMtx[TrinBT,2]) #lon1,lat1,lon2,lat2
        TriBT2BTMtx[i, TrinBT] <<- TriBT2BTMtx[TrinBT, i] 
        
        #DEBUG
        cat("****> Boat:", TrinBT,"and boat:" ,i,"coords",TriBTMtx[i,],"\n")
        
        #DEBUG if (i==1) cat("Dist between ",i, "at (",dx1,dy1,") and",j, "is",dist,"\n")
        #DEBUG cat ("Dist = ",dist,"\n")
        
        
        
        #Now check the boats that are "close" to each other
        #They must be at max rescueDistance=currentDroneRange / 2 away
        #Only after that you can check the possibility of a yellow edge
        #Connect the boat to boat that can be reached
        yellowDist <<- floor(currentDroneRange / 2)
        if (TriBT2BTMtx[TrinBT, i] < yellowDist) {
          
          #Now we have two boats very close to each other
          #We need to analyze the distance between them the red and gray edges that they have etc...
          #Note that we can have single or 2,3,... levels of yellow edges
          
          #Constraints:
          #But the total should always be less than or equal to drone range.
          #The chain generally contains red and gray edge.
          # So sum(yellow edges) + red + gray <= drone range
          # or sum(yellow edges) + gray + gray <= drone range
          # or sum(yellow edges) + gray + red <= drone range
          
          #Policies:
          #We can choose the longest possible yellow edge chain
          #We can choose the shortest possible yellow edge chain
          #etc...
          
          
          #HERE WE JUST ADD THE POSSIBLE YELLOW EDGES
          #WE DECIDE LATER WHICH ONES TO USE
          
          #Find the min gray/red edge to depot from the adjacent Boat(i) - Adj
          AdjMinGrayEdgeW <- min(BT2TriDTedgeW[i, which(BT2TriDTGrayEdge[i,] == 1)])
          AdjMinGrayEdgeTriDTno <- which(BT2TriDTedgeW[i,] == AdjMinGrayEdgeW)
          
          AdjMinRedEdgeW <- min(BT2TriDTedgeW[i, which(BT2TriDTRedEdge[i,] == 1)])
          AdjMinRedEdgeTriDTno <- which(BT2TriDTedgeW[i,] == AdjMinRedEdgeW)
          
          cat("*** YELLOW SEARCH - ADJ AdjMinGrayEdgeW:",AdjMinGrayEdgeW, "AdjMinGrayEdgeTriDTno:", AdjMinGrayEdgeTriDTno,"\n")
          cat("*** YELLOW SEARCH - ADJ AdjMinRedEdgeW:",AdjMinRedEdgeW, "AdjMinRedEdgeTriDTno:", AdjMinRedEdgeTriDTno,"\n")
          
          #Find the min gray/red edge to depot from this Boat(TrinBT) - This
          ThisMinGrayEdgeW <- min(BT2TriDTedgeW[TrinBT, which(BT2TriDTGrayEdge[TrinBT,] == 1)])
          ThisMinGrayEdgeTriDTno <- which(BT2TriDTedgeW[TrinBT,] == ThisMinGrayEdgeW)
          
          ThisMinRedEdgeW <- min(BT2TriDTedgeW[TrinBT, which(BT2TriDTRedEdge[TrinBT,] == 1)])
          ThisMinRedEdgeTriDTno <- which(BT2TriDTedgeW[TrinBT,] == ThisMinRedEdgeW)
          
          cat("*** YELLOW SEARCH - THIS ThisMinGrayEdgeW:",ThisMinGrayEdgeW, "ThisMinGrayEdgeTriDTno:", ThisMinGrayEdgeTriDTno,"\n")
          cat("*** YELLOW SEARCH - THIS ThisMinRedEdgeW:",ThisMinRedEdgeW, "ThisMinRedEdgeTriDTno:", ThisMinRedEdgeTriDTno,"\n")
          
          
          #Do we have yellow edge if we use only gray edges?
          if ( (ThisMinGrayEdgeW + AdjMinGrayEdgeW + TriBT2BTMtx[TrinBT, i]) <= currentDroneRange) {
            
            #BT2TriDTAdj[i,j] <- 1
            TriBT2BTAdj[TrinBT,i] <<- 1
            TriBT2BTAdj[i, TrinBT] <<- 1
            
            TriBT2BTlinesMTX <<- rbind(TriBT2BTlinesMTX, TriBTMtx[i,]) 
            TriBT2BTlinesMTX <<- rbind(TriBT2BTlinesMTX, TriBTMtx[TrinBT,])
            
            TriBT2BTlinesBuffer <- rbind(TriBT2BTlinesBuffer, TriBTMtx[i,]) 
            TriBT2BTlinesBuffer <- rbind(TriBT2BTlinesBuffer, TriBTMtx[TrinBT,])
            
            
            #update edge weights too
            #BT2TriDTedgeW[i,j] <- BT2TriDTMtx[i,j]
            TriBT2BTedgeW[TrinBT,i] <<- TriBT2BTMtx[TrinBT,i]
            
            
            
            #DEBUG
            cat("YELLOW Reachable Boats: Connecting boat", i, "to boat no", TrinBT ,
                "dist:", TriBT2BTMtx[TrinBT, i], "m or", round(TriBT2BTMtx[TrinBT, i]/currentDroneRange, 3) , "unit.\n")
            gTriRedGrayYellow <<- gTriRedGrayYellow %>% add_edges(c(nTriVertex, (nBS + nTriDT + i)), color=yellowEdgeCol, width = 1, weight = TriBT2BTedgeW[TrinBT,i])
            #Maybe gWithoutYellow?
            #gWithoutRed <<- gWithoutRed %>% add_edges(c(nVertex,i), color=grayEdgeCol, width = 1, weight = BT2TriDTedgeW[TrinBT,i])
            
            # if(input$edgeFilter=="gr"){
            #   myGraph$value <- gTriGray
            # }else if(input$edgeFilter=="rg"){
            #   myGraph$value <- gTriRedGray
            # }else if(input$edgeFilter=="rgy"){
            #   myGraph$value <- gTriRedGrayYellow
            # }
            
            
          }
          
          
          
          
        } else if(TriBT2BTMtx[TrinBT, i] <= currentDroneRange) {
          
          # For the time being lets do nothing for this case
          
          # BT2TriDTAdj[TrinBT,i] <<- 1
          # 
          # BT2TriDTlinesMTX <<- rbind(BT2TriDTlinesMTX, TriDTMtx[i,]) 
          # BT2TriDTlinesMTX <<- rbind(BT2TriDTlinesMTX, TriBTMtx[TrinBT,])
          # 
          # BT2TriDTlinesBuffer <- rbind(BT2TriDTlinesBuffer, TriDTMtx[i,]) 
          # BT2TriDTlinesBuffer <- rbind(BT2TriDTlinesBuffer, TriBTMtx[TrinBT,])
          # 
          # 
          # #update edge weights too
          # #BT2TriDTedgeW[i,j] <- BT2TriDTMtx[i,j]
          # BT2TriDTedgeW[TrinBT,i] <<- BT2TriDTMtx[TrinBT,i]
          # 
          # 
          # 
          # #DEBUG
          # cat("RED Reachable: Connecting vertex", nVertex, "Boat no", TrinBT ,"to depot", i,"\n")
          # gRedGray <<- gRedGray %>% add_edges(c(nVertex,i), color=redEdgeCol, width = 1, weight = BT2TriDTedgeW[TrinBT,i])
          # myGraph$value <- gRedGray
          
          
        } #if
      } #for
    }else{
      cat("Single Boat\n")
    }  #if (TrinBT >= 2)
    
    
    ####################################### 
    #END loop for among boats
    ####################################### 
    
    
    #######################################
    # BEGIN update active data structures
    ####################################### 
    # Now after BT update, for the active grid the related 
    # BT2DT
    # BT2BT
    # arrays should be updated!!!
    ####################################### 
    
    
    if (activeGridType == "tri") {
      # BT2DT
      ####################################### 
      #Boat to Depot data structures
      #Active
      
      BT2DTlinesMTX  <<-  BT2TriDTlinesMTX
      BT2DTMtx <<- BT2TriDTMtx
      BT2DTAdj <<- BT2TriDTAdj
      BT2DTedgeW <<- BT2TriDTedgeW
      BT2DTGrayEdge <<- BT2TriDTGrayEdge
      BT2DTRedEdge <<- BT2TriDTRedEdge
      
      
      # BT2BT
      #######################################
      #Boat to Boat data structures
      #Active
      
      BT2BTlinesMTX  <<-  TriBT2BTlinesMTX
      BT2BTMtx <<- TriBT2BTMtx
      BT2BTAdj <<- TriBT2BTAdj 
      BT2BTedgeW <<-  TriBT2BTedgeW 
    }
    
    
    #######################################
    # END update active data structures
    ####################################### 
    
    
    
    ####################################################################  
    #Update the graph and the plot
    ####################################################################      
    # We need lines between depots and boats
    ####################################################################
    
    adjSum <- sum(BT2TriDTAdj[TrinBT,])
    if (adjSum == 0) {
      cat("Boat",TrinBT, "is not reachable!\n")
      #DEBUG
      cat("Added isolated vertex:", nTriVertex,"with label",paste0("B",TrinBT), "\n")
      #gRedGray <<- gRedGray %>% add_vertices(1, color="blue", label = paste0("B",TrinBT), label.cex = 0.3) 
      #myGraph$value <- gRedGray
      #%>% add_edges(c(nVertex,i, i,nVertex), color=grayEdgeCol, width = 1, weight = Inf)
    }else{
      cat("Boat",TrinBT, "is reachable from",adjSum,"depots!\n")
    }
    
    cat("From addBoatTri: The selected grid type is",activeGridType,"\n")
    
    if (activeGridType == "tri") {
      #Draw all the lines for "adjacent" depots
      if (adjSum > 1){
        for (k in seq(1, (2*adjSum-1), by = 2)) {
          df <- as.data.frame(rbind(BT2TriDTlinesBuffer[k,], BT2TriDTlinesBuffer[k+1,]))
          colnames(df) <- c("x","y", "edgeColor")
          df$x <- as.numeric(df$x)
          df$y <- as.numeric(df$y)
          
          leafletProxy("mymap") %>% 
            addPolylines(data = df,
                         lng = ~ x, 
                         lat = ~ y,
                         color = ~ edgeColor,
                         weight = 2,
                         group = "BT2DTEdges")
          
          
          mapToSave$current <- mapToSave$base %>% 
            addPolylines(data = df,
                         lng = ~ x, 
                         lat = ~ y,
                         color = ~ edgeColor,
                         weight = 2,
                         group = "BT2DTEdges")
        }
        
        
      }else if (adjSum == 1){
        df <- as.data.frame(rbind(BT2TriDTlinesBuffer[1,], BT2TriDTlinesBuffer[2,]))
        colnames(df) <- c("x","y", "edgeColor")
        df$x <- as.numeric(df$x)
        df$y <- as.numeric(df$y)
        
        leafletProxy("mymap") %>% 
          addPolylines(data = df,
                       lng = ~ x, 
                       lat = ~ y,
                       color = ~ edgeColor,
                       weight = 2,
                       group = "BT2DTEdges")
        
        
        mapToSave$current <- mapToSave$base %>% 
          addPolylines(data = df,
                       lng = ~ x, 
                       lat = ~ y,
                       color = ~ edgeColor,
                       weight = 2,
                       group = "BT2DTEdges")
      }else{
        cat("No lines from depots. Boat",TrinBT, "is not reachable!\n")
      }
    }
    
    
    ####################################################################   
    cat("addBoatTri: Boat", TrinBT,"is added bye!\n")
    
  }
  
  ########################################################################################################
  
  
  
  ######################################################################################################## 
  #Adds single boat to Sq Grid!!!
  #Make sure before calling this function that the boat is reachable for all Grid types!!!
  addBoatSq <- function(boatPos, btIDX){
    
    
    
    currentDroneRange <- input$drone_range
    btLng <- boatPos[btIDX,]$x
    btLat <- boatPos[btIDX,]$y
    
    
    SqnBT <<- SqnBT + 1
    nSqVertex <<- nBS + nSqDT + SqnBT
    
    #DEBUG
    cat("Adding boat", btIDX,"at", btLng, btLat," to Sq Grid.\n")
    cat("Drone range",currentDroneRange," meters. In polygon Vertex no:",nSqVertex,"Boat No:",SqnBT,"\n")
    
    #LATER nBSandSqDepot  <<- nBSandSqDepot + 1
    
    
    
    text2 <- paste0("BT point[", SqnBT,"] ", btIDX, "at: ", btLng , btLat)
    
    cat("Clicked point is in the region. Added BT point - Total points:", SqnBT,"\n")
    SqBTMtx <<- rbind(SqBTMtx, c(btLng, btLat))
    SqVTXMtx <<- rbind(SqVTXMtx, c(btLng, btLat))
    #vertexPositions <<- rbind(vertexPositions, c(btLng, btLat))
    
    
    
    
    ###########################################################################
    #Here lets add it to graph!!!
    #Add the boat to BT2SqDTMtx first as a row
    
    ###########################################################################
    #We add the boats to the graph labeling them after nBS+nDT (BSs + depots)
    #But for them the adjacency condition is that they are at the 
    #rescueDist from some depots.
    ###########################################################################
    
    
    
    
    ###########################################################################
    #Init and modify some data structures of Boats and Depots
    ###########################################################################
    #Let find the adjacency mtx for depots + BS (index 1 is for BS) + boats
    BT2SqDTAdj <<- rbind(BT2SqDTAdj, matrix(0, nrow=1, ncol=nBSandSqDepot))
    #Adj for gray edges
    BT2SqDTGrayEdge <<- rbind(BT2SqDTGrayEdge, matrix(0, nrow=1, ncol=nBSandSqDepot))
    #Adj for red edges
    BT2SqDTRedEdge <<- rbind(BT2SqDTRedEdge, matrix(0, nrow=1, ncol=nBSandSqDepot))
    #These mtx s have same size!!!
    
    
    nr <- dim(SqBT2BTAdj)[1]
    if (is.null(nr)) { nr <- 1} 
    #Add the last column and then the last row to "expand" the adj matrix
    SqBT2BTAdj <<- cbind(SqBT2BTAdj, matrix(0, nrow=nr, ncol=1))
    SqBT2BTAdj <<- rbind(SqBT2BTAdj, matrix(0, nrow=1, ncol=SqnBT))
    
    
    
    
    #Well save the distances as the "weights" for edges. Initially all are infinity
    BT2SqDTedgeW <<- rbind(BT2SqDTedgeW, matrix(Inf, nrow=1, ncol=nBSandSqDepot))
    
    
    nr <- dim(SqBT2BTedgeW)[1]
    if (is.null(nr)) { nr <- 1} 
    SqBT2BTedgeW <<- cbind(SqBT2BTedgeW, matrix(Inf, nrow=nr, ncol=1))
    SqBT2BTedgeW <<- rbind(SqBT2BTedgeW, matrix(Inf, nrow=1, ncol=SqnBT))
    
    
    
    #Add the row for that Boat and the vertex to the graph
    BT2SqDTMtx <<- rbind(BT2SqDTMtx, matrix(Inf, nrow = 1, ncol = nBSandSqDepot))
    
    
    nr <- dim(SqBT2BTMtx)[1]
    if (is.null(nr)) { nr <- 1} 
    SqBT2BTMtx <<- cbind(SqBT2BTMtx, matrix(Inf, nrow = nr, ncol = 1))
    SqBT2BTMtx <<- rbind(SqBT2BTMtx, matrix(Inf, nrow = 1, ncol = SqnBT))
    
    #DEBUG
    cat("Adding vertex:", nSqVertex, "with label:", paste0("B",SqnBT),"\n")
    gSqRedGray <<- gSqRedGray             %>% add_vertices(1, color="lightblue", shape = "myBT", name = nSqVertex, label = paste0("B",SqnBT), label.cex = 0.2)
    gSqRedGrayYellow <<- gSqRedGrayYellow %>% add_vertices(1, color="lightblue", shape = "myBT", name = nSqVertex, label = paste0("B",SqnBT), label.cex = 0.2)
    gSqGray <<- gSqGray                   %>% add_vertices(1, color="lightblue", shape = "myBT", name = nSqVertex, label = paste0("B",SqnBT), label.cex = 0.2)
    
    
    # if(input$edgeFilter=="gr"){
    #   myGraph$value <- gSqGray
    # }else if(input$edgeFilter=="rg"){
    #   myGraph$value <- gSqRedGray
    # }else if(input$edgeFilter=="rgy"){
    #   myGraph$value <- gSqRedGrayYellow
    # }
    
    
    #Buffer the lines for the current boat and plot them at the end.
    BT2SqDTlinesBuffer <- NULL
    SqBT2BTlinesBuffer <- NULL
    
    
    ########################################
    #BEGIN loops for between depots and boats
    ########################################  
    
    ########################################
    #First add gray edges
    #Check the distance to all depots
    ########################################
    
    rescueDist <<- floor(currentDroneRange / 2) # + rangeDelta
    
    
    for (i in (1:nBSandSqDepot)){
      
      #Find the dist in meters between the boats and the depots
      BT2SqDTMtx[SqnBT, i] <<- lonlat2m(SqDTMtx[i,1], SqDTMtx[i,2], SqBTMtx[SqnBT,1], SqBTMtx[SqnBT,2]) #lon1,lat1,lon2,lat2
      
      
      #DEBUG
      #cat("**** Boat:", SqnBT,"depot:" ,i,"coords",SqDTMtx[i,],"\n")
      
      #DEBUG if (i==1) cat("Dist between ",i, "at (",dx1,dy1,") and",j, "is",dist,"\n")
      #DEBUG cat ("Dist = ",dist,"\n")
      
      #Connect the boat to depot that can be reached
      
      if (BT2SqDTMtx[SqnBT, i] <= rescueDist) {
        ####################################
        #GRAY edges here
        ####################################
        #BT2SqDTAdj[i,j] <- 1
        BT2SqDTAdj[SqnBT,i] <<- 1
        
        BT2SqDTGrayEdge[SqnBT,i] <<- 1
        
        
        BT2SqDTlinesMTX <<- rbind(BT2SqDTlinesMTX, SqDTMtx[i,]) 
        BT2SqDTlinesMTX <<- rbind(BT2SqDTlinesMTX, SqBTMtx[SqnBT,])
        
        BT2SqDTlinesBuffer <- rbind(BT2SqDTlinesBuffer, c(SqDTMtx[i,], "gray")) 
        BT2SqDTlinesBuffer <- rbind(BT2SqDTlinesBuffer, c(SqBTMtx[SqnBT,], "gray"))
        
        
        #update edge weights too
        #BT2SqDTedgeW[i,j] <- BT2SqDTMtx[i,j]
        BT2SqDTedgeW[SqnBT,i] <<- BT2SqDTMtx[SqnBT,i]
        
        
        
        #DEBUG
        cat("GRAY Reachable: Connecting vertex", nSqVertex, "Boat no", SqnBT ,"to depot", i,"dist=",BT2SqDTedgeW[SqnBT,i],"\n")
        gSqRedGray <<- gSqRedGray             %>% add_edges(c(nSqVertex,i), color=grayEdgeCol, width = 1, weight = BT2SqDTedgeW[SqnBT,i])
        gSqRedGrayYellow <<- gSqRedGrayYellow %>% add_edges(c(nSqVertex,i), color=grayEdgeCol, width = 1, weight = BT2SqDTedgeW[SqnBT,i])
        gSqGray <<- gSqGray                   %>% add_edges(c(nSqVertex,i), color=grayEdgeCol, width = 1, weight = BT2SqDTedgeW[SqnBT,i])
        
        # if(input$edgeFilter=="gr"){
        #   myGraph$value <- gSqGray
        # }else if(input$edgeFilter=="rg"){
        #   myGraph$value <- gSqRedGray
        # }else if(input$edgeFilter=="rgy"){
        #   myGraph$value <- gSqRedGrayYellow
        # }
      } #if for gray edge
    } #end of for loop over the depots
    
    
    ########################################
    #Then red edges
    #Check the distance to all depots
    ########################################
    
    for (i in (1:nBSandSqDepot)){
      
      
      #TODO
      #Here you need an vector that shows you if the boat has gray edge
      
      nGrayEdge <- sum(BT2SqDTGrayEdge[SqnBT,])
      
      if (nGrayEdge >= 1) {
        minGrayEdgeWeight <- min(BT2SqDTedgeW[SqnBT,which(BT2SqDTGrayEdge[SqnBT,] == 1)])
        minGrayEdgeSrcVtxId <- which(BT2SqDTedgeW[SqnBT,] == minGrayEdgeWeight)
        
        
        if ( (BT2SqDTRedEdge[SqnBT, i] == 0) && 
             (BT2SqDTGrayEdge[SqnBT, i] == 0) && 
             ((BT2SqDTMtx[SqnBT,i] + minGrayEdgeWeight) <= currentDroneRange) ) {
          
          ################################################################
          #RED edges here
          ################################################################
          
          BT2SqDTAdj[SqnBT,i] <<- 1
          BT2SqDTRedEdge[SqnBT,i] <<- 1
          
          BT2SqDTlinesMTX <<- rbind(BT2SqDTlinesMTX, SqDTMtx[i,]) 
          BT2SqDTlinesMTX <<- rbind(BT2SqDTlinesMTX, SqBTMtx[SqnBT,])
          
          BT2SqDTlinesBuffer <- rbind(BT2SqDTlinesBuffer, c(SqDTMtx[i,],"red")) 
          BT2SqDTlinesBuffer <- rbind(BT2SqDTlinesBuffer, c(SqBTMtx[SqnBT,],"red"))
          
          
          #update edge weights too
          #BT2SqDTedgeW[i,j] <- BT2SqDTMtx[i,j]
          BT2SqDTedgeW[SqnBT,i] <<- BT2SqDTMtx[SqnBT,i]
          
          
          
          #DEBUG
          cat("RED Reachable: Connecting vertex", nSqVertex, "Boat no", SqnBT ,"to depot", i,"dist=",BT2SqDTedgeW[SqnBT,i],"\n")
          gSqRedGray <<- gSqRedGray             %>% add_edges(c(nSqVertex,i), color=redEdgeCol, width = 1, weight = BT2SqDTedgeW[SqnBT,i])
          gSqRedGrayYellow <<- gSqRedGrayYellow %>% add_edges(c(nSqVertex,i), color=redEdgeCol, width = 1, weight = BT2SqDTedgeW[SqnBT,i])
          
          # if(input$edgeFilter=="gr"){
          #   myGraph$value <- gSqGray
          # }else if(input$edgeFilter=="rg"){
          #   myGraph$value <- gSqRedGray
          # }else if(input$edgeFilter=="rgy"){
          #   myGraph$value <- gSqRedGrayYellow
          # }
          
        }# if for red edge
        
      }else{
        
        cat("No GRAY edge for", nSqVertex, "Boat no", SqnBT,"\n")
      }# else no gray edge
      
    } #end of for loop over the depots
    
    
    
    
    
    
    
    
    
    
    
    #######################################         
    #END loops for between depots and boats
    #######################################   
    
    
    
    
    ######################################################### 
    #BEGIN loop for among boats. IF YOU HAVE AT LEAST 2 BOATS
    ######################################################### 
    
    #Check the current boat "SqnBT" against all the others
    if (SqnBT >= 2) {
      for (i in (1:(SqnBT-1))){
        
        #Find the dist in meters between two boats
        SqBT2BTMtx[SqnBT, i] <<- lonlat2m(SqBTMtx[i,1], SqBTMtx[i,2], SqBTMtx[SqnBT,1], SqBTMtx[SqnBT,2]) #lon1,lat1,lon2,lat2
        
        
        #DEBUG
        cat("****> Boat:", SqnBT,"and boat:" ,i,"coords",SqBTMtx[i,],"\n")
        
        #DEBUG if (i==1) cat("Dist between ",i, "at (",dx1,dy1,") and",j, "is",dist,"\n")
        #DEBUG cat ("Dist = ",dist,"\n")
        
        #Connect the boat to boat that can be reached
        yellowDist <<- floor(currentDroneRange / 2)
        if (SqBT2BTMtx[SqnBT, i] <= yellowDist) {
          #BT2SqDTAdj[i,j] <- 1
          SqBT2BTAdj[SqnBT,i] <<- 1
          
          SqBT2BTlinesMTX <<- rbind(SqBT2BTlinesMTX, SqBTMtx[i,]) 
          SqBT2BTlinesMTX <<- rbind(SqBT2BTlinesMTX, SqBTMtx[SqnBT,])
          
          SqBT2BTlinesBuffer <- rbind(SqBT2BTlinesBuffer, SqBTMtx[i,]) 
          SqBT2BTlinesBuffer <- rbind(SqBT2BTlinesBuffer, SqBTMtx[SqnBT,])
          
          
          #update edge weights too
          #BT2SqDTedgeW[i,j] <- BT2SqDTMtx[i,j]
          SqBT2BTedgeW[SqnBT,i] <<- SqBT2BTMtx[SqnBT,i]
          
          
          
          #DEBUG
          cat("YELLOW Reachable Boats: Connecting boat", i, "to boat no", SqnBT ,"\n")
          gSqRedGrayYellow <<- gSqRedGrayYellow %>% add_edges(c(nSqVertex, (nBS + nSqDT + i)), color=yellowEdgeCol, width = 1, weight = SqBT2BTedgeW[SqnBT,i])
          #Maybe gWithoutYellow?
          #gWithoutRed <<- gWithoutRed %>% add_edges(c(nVertex,i), color=grayEdgeCol, width = 1, weight = BT2SqDTedgeW[SqnBT,i])
          
          # if(input$edgeFilter=="gr"){
          #   myGraph$value <- gSqGray
          # }else if(input$edgeFilter=="rg"){
          #   myGraph$value <- gSqRedGray
          # }else if(input$edgeFilter=="rgy"){
          #   myGraph$value <- gSqRedGrayYellow
          # }
        } else if(SqBT2BTMtx[SqnBT, i] <= currentDroneRange) {
          
          # For the time being lets do nothing for this case
          
          # BT2SqDTAdj[SqnBT,i] <<- 1
          # 
          # BT2SqDTlinesMTX <<- rbind(BT2SqDTlinesMTX, SqDTMtx[i,]) 
          # BT2SqDTlinesMTX <<- rbind(BT2SqDTlinesMTX, SqBTMtx[SqnBT,])
          # 
          # BT2SqDTlinesBuffer <- rbind(BT2SqDTlinesBuffer, SqDTMtx[i,]) 
          # BT2SqDTlinesBuffer <- rbind(BT2SqDTlinesBuffer, SqBTMtx[SqnBT,])
          # 
          # 
          # #update edge weights too
          # #BT2SqDTedgeW[i,j] <- BT2SqDTMtx[i,j]
          # BT2SqDTedgeW[SqnBT,i] <<- BT2SqDTMtx[SqnBT,i]
          # 
          # 
          # 
          # #DEBUG
          # cat("RED Reachable: Connecting vertex", nVertex, "Boat no", SqnBT ,"to depot", i,"\n")
          # gRedGray <<- gRedGray %>% add_edges(c(nVertex,i), color=redEdgeCol, width = 1, weight = BT2SqDTedgeW[SqnBT,i])
          # myGraph$value <- gRedGray
          
          
        } #if
      }
    }else{
      cat("Single Boat\n")
    }
    
    
    ####################################### 
    #END loop for among boats
    ####################################### 
    
    
    
    
    
    #######################################
    # BEGIN update active data structures
    ####################################### 
    # Now after BT update, for the active grid the related 
    # BT2DT
    # BT2BT
    # arrays should be updated!!!
    ####################################### 
    
    
    if (activeGridType == "sq") {
      # BT2DT
      ####################################### 
      #Boat to Depot data structures
      #Active
      
      BT2DTlinesMTX  <<-  BT2SqDTlinesMTX
      BT2DTMtx <<- BT2SqDTMtx
      BT2DTAdj <<- BT2SqDTAdj
      BT2DTedgeW <<- BT2SqDTedgeW
      BT2DTGrayEdge <<- BT2SqDTGrayEdge
      BT2DTRedEdge <<- BT2SqDTRedEdge
      
      
      # BT2BT
      #######################################
      #Boat to Boat data structures
      #Active
      
      BT2BTlinesMTX  <<-  SqBT2BTlinesMTX
      BT2BTMtx <<- SqBT2BTMtx
      BT2BTAdj <<- SqBT2BTAdj 
      BT2BTedgeW <<-  SqBT2BTedgeW 
    }
    
    
    #######################################
    # END update active data structures
    ####################################### 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ####################################################################  
    #Update the graph and the plot
    ####################################################################      
    # We need lines between depots and boats
    ####################################################################
    
    adjSum <- sum(BT2SqDTAdj[SqnBT,])
    if (adjSum == 0) {
      cat("Boat",SqnBT, "is not reachable!\n")
      #DEBUG
      cat("Added isolated vertex:", nSqVertex,"with label",paste0("B",SqnBT), "\n")
      #gRedGray <<- gRedGray %>% add_vertices(1, color="blue", label = paste0("B",SqnBT), label.cex = 0.3) 
      #myGraph$value <- gRedGray
      #%>% add_edges(c(nVertex,i, i,nVertex), color=grayEdgeCol, width = 1, weight = Inf)
    }else{
      cat("Boat",SqnBT, "is reachable from",adjSum,"depots!\n")
    }
    
    
    cat("From addBoatSq: The selected grid type is",activeGridType,"\n")
    if (activeGridType == "sq") {
      
      #Draw all the lines for "adjacent" depots
      if (adjSum > 1){
        for (k in seq(1, (2*adjSum-1), by = 2)) {
          df <- as.data.frame(rbind(BT2SqDTlinesBuffer[k,], BT2SqDTlinesBuffer[k+1,]))
          colnames(df) <- c("x","y", "edgeColor")
          df$x <- as.numeric(df$x)
          df$y <- as.numeric(df$y)
          
          leafletProxy("mymap") %>% 
            addPolylines(data = df,
                         lng = ~ x, 
                         lat = ~ y,
                         color = ~ edgeColor,
                         weight = 2,
                         group = "BT2DTEdges")
          
          
          mapToSave$current <- mapToSave$base %>% 
            addPolylines(data = df,
                         lng = ~ x, 
                         lat = ~ y,
                         color = ~ edgeColor,
                         weight = 2,
                         group = "BT2DTEdges")
        }
        
        
      }else if (adjSum == 1){
        df <- as.data.frame(rbind(BT2SqDTlinesBuffer[1,], BT2SqDTlinesBuffer[2,]))
        colnames(df) <- c("x","y", "edgeColor")
        df$x <- as.numeric(df$x)
        df$y <- as.numeric(df$y)
        
        leafletProxy("mymap") %>% 
          addPolylines(data = df,
                       lng = ~ x, 
                       lat = ~ y,
                       color = ~ edgeColor,
                       weight = 2,
                       group = "BT2DTEdges")
        
        
        mapToSave$current <- mapToSave$base %>% 
          addPolylines(data = df,
                       lng = ~ x, 
                       lat = ~ y,
                       color = ~ edgeColor,
                       weight = 2,
                       group = "BT2DTEdges")
      }else{
        cat("No lines from depots. Boat",SqnBT, "is not reachable!\n")
      }
    }
    
    ####################################################################   
    
    cat("addBoatSq: Boat", SqnBT,"is added bye!\n")
  }
  
  ########################################################################################################
  
  
  
  
  
  ######################################################################################################## 
  #Removes single boat !!!
  removeBoat <- function(btNo){
    
    
    
    currentDroneRange <- input$drone_range
    btLng <- boatPos[btIDX,]$x
    btLat <- boatPos[btIDX,]$y
    
    
    nBT <<- nBT + 1
    nVertex <<- nBS + nDT + nBT
    
    #DEBUG
    cat("Removing Boat", btNo,"at",BTMtx[btNo,],"\n")
    cat("Drone range",currentDroneRange," meters. In polygon Vertex no:",nVertex,"Boats:",nBT,"\n")
    
    
    
    
    
    text2 <- paste0("BT point[", nBT,"] ", btIDX, "at: ", btLng , btLat)
    
    cat("Clicked point is in the region. Added BT point - Total points:", nBT,"\n")
    BTMtx <<- rbind(BTMtx, c(btLng, btLat))
    VTXMtx <<- rbind(VTXMtx, c(btLng, btLat))
    #vertexPositions <<- rbind(vertexPositions, c(btLng, btLat))
    
    
    
    
    ###########################################################################
    #Here lets add it to graph!!!
    #Add the boat to BT2DTMtx first as a row
    
    ###########################################################################
    #We add the boats to the graph labeling them after nBS+nDT (BSs + depots)
    #But for them the adjacency condition is that they are at the 
    #rescueDist from some depots.
    ###########################################################################
    
    
    
    
    ###########################################################################
    #Init and modify some data structures of Boats and Depots
    ###########################################################################
    #Let find the adjacency mtx for depots + BS (index 1 is for BS) + boats
    BT2DTAdj <<- rbind(BT2DTAdj, matrix(0, nrow=1, ncol=nBSandDepot))
    #Adj for gray edges
    BT2DTGrayEdge <<- rbind(BT2DTGrayEdge, matrix(0, nrow=1, ncol=nBSandDepot))
    #Adj for red edges
    BT2DTRedEdge <<- rbind(BT2DTRedEdge, matrix(0, nrow=1, ncol=nBSandDepot))
    #These mtx s have same size!!!
    
    
    nr <- dim(BT2BTAdj)[1]
    if (is.null(nr)) { nr <- 1} 
    #Add the last column and then the last row to "expand" the adj matrix
    BT2BTAdj <<- cbind(BT2BTAdj, matrix(0, nrow=nr, ncol=1))
    BT2BTAdj <<- rbind(BT2BTAdj, matrix(0, nrow=1, ncol=nBT))
    
    
    
    
    #Well save the distances as the "weights" for edges. Initially all are infinity
    BT2DTedgeW <<- rbind(BT2DTedgeW, matrix(Inf, nrow=1, ncol=nBSandDepot))
    
    
    nr <- dim(BT2BTedgeW)[1]
    if (is.null(nr)) { nr <- 1} 
    BT2BTedgeW <<- cbind(BT2BTedgeW, matrix(Inf, nrow=nr, ncol=1))
    BT2BTedgeW <<- rbind(BT2BTedgeW, matrix(Inf, nrow=1, ncol=nBT))
    
    
    
    #Add the row for that Boat and the vertex to the graph
    BT2DTMtx <<- rbind(BT2DTMtx, matrix(Inf, nrow = 1, ncol = nBSandDepot))
    
    
    nr <- dim(BT2BTMtx)[1]
    if (is.null(nr)) { nr <- 1} 
    BT2BTMtx <<- cbind(BT2BTMtx, matrix(Inf, nrow = nr, ncol = 1))
    BT2BTMtx <<- rbind(BT2BTMtx, matrix(Inf, nrow = 1, ncol = nBT))
    
    #DEBUG
    cat("Adding vertex:", nVertex, "with label:", paste0("B",nBT),"\n")
    gRedGray <<- gRedGray             %>% add_vertices(1, color="lightblue", shape = "myBT", name =  nVertex, label = paste0("B",nBT), label.cex = 0.2)
    gRedGrayYellow <<- gRedGrayYellow %>% add_vertices(1, color="lightblue", shape = "myBT", name =  nVertex, label = paste0("B",nBT), label.cex = 0.2)
    gGray <<- gGray                   %>% add_vertices(1, color="lightblue", shape = "myBT", name =  nVertex, label = paste0("B",nBT), label.cex = 0.2)
    
    
    if(input$edgeFilter=="gr"){
      myGraph$value <- gGray
    }else if(input$edgeFilter=="rg"){
      myGraph$value <- gRedGray
    }else if(input$edgeFilter=="rgy"){
      myGraph$value <- gRedGrayYellow
    }
    
    
    #Buffer the lines for the current boat and plot them at the end.
    BT2DTlinesBuffer <- NULL
    BT2BTlinesBuffer <- NULL
    
    
    ########################################
    #BEGIN loops for between depots and boats
    ########################################  
    
    ########################################
    #First add gray edges
    #Check the distance to all depots
    ########################################
    
    rescueDist <<- floor(currentDroneRange / 2) # + rangeDelta
    
    
    for (i in (1:nBSandDepot)){
      
      #Find the dist in meters between the boats and the depots
      BT2DTMtx[nBT, i] <<- lonlat2m(DTMtx[i,1], DTMtx[i,2], BTMtx[nBT,1], BTMtx[nBT,2]) #lon1,lat1,lon2,lat2
      
      
      #DEBUG
      #cat("**** Boat:", nBT,"depot:" ,i,"coords",DTMtx[i,],"\n")
      
      #DEBUG if (i==1) cat("Dist between ",i, "at (",dx1,dy1,") and",j, "is",dist,"\n")
      #DEBUG cat ("Dist = ",dist,"\n")
      
      #Connect the boat to depot that can be reached
      
      if (BT2DTMtx[nBT, i] <= rescueDist) {
        ####################################
        #GRAY edges here
        ####################################
        #BT2DTAdj[i,j] <- 1
        BT2DTAdj[nBT,i] <<- 1
        
        BT2DTGrayEdge[nBT,i] <<- 1
        
        
        BT2DTlinesMTX <<- rbind(BT2DTlinesMTX, DTMtx[i,]) 
        BT2DTlinesMTX <<- rbind(BT2DTlinesMTX, BTMtx[nBT,])
        
        BT2DTlinesBuffer <- rbind(BT2DTlinesBuffer, c(DTMtx[i,], "gray")) 
        BT2DTlinesBuffer <- rbind(BT2DTlinesBuffer, c(BTMtx[nBT,], "gray"))
        
        
        #update edge weights too
        #BT2DTedgeW[i,j] <- BT2DTMtx[i,j]
        BT2DTedgeW[nBT,i] <<- BT2DTMtx[nBT,i]
        
        
        
        #DEBUG
        cat("GRAY Reachable: Connecting vertex", nVertex, "Boat no", nBT ,"to depot", i,"dist=",BT2DTedgeW[nBT,i],"\n")
        gRedGray <<- gRedGray             %>% add_edges(c(nVertex,i), color=grayEdgeCol, width = 1, weight = BT2DTedgeW[nBT,i])
        gRedGrayYellow <<- gRedGrayYellow %>% add_edges(c(nVertex,i), color=grayEdgeCol, width = 1, weight = BT2DTedgeW[nBT,i])
        gGray <<- gGray                   %>% add_edges(c(nVertex,i), color=grayEdgeCol, width = 1, weight = BT2DTedgeW[nBT,i])
        
        if(input$edgeFilter=="gr"){
          myGraph$value <- gGray
        }else if(input$edgeFilter=="rg"){
          myGraph$value <- gRedGray
        }else if(input$edgeFilter=="rgy"){
          myGraph$value <- gRedGrayYellow
        }
      } #if for gray edge
    } #end of for loop over the depots
    
    
    ########################################
    #Then red edges
    #Check the distance to all depots
    ########################################
    
    for (i in (1:nBSandDepot)){
      
      
      #TODO
      #Here you need an vector that shows you if the boat has gray edge
      
      nGrayEdge <- sum(BT2DTGrayEdge[nBT,])
      
      if (nGrayEdge >= 1) {
        minGrayEdgeWeight <- min(BT2DTedgeW[nBT,which(BT2DTGrayEdge[nBT,] == 1)])
        minGrayEdgeSrcVtxId <- which(BT2DTedgeW[nBT,] == minGrayEdgeWeight)
        
        
        if ( (BT2DTRedEdge[nBT, i] == 0) && 
             (BT2DTGrayEdge[nBT, i] == 0) && 
             ((BT2DTMtx[nBT,i] + minGrayEdgeWeight) <= currentDroneRange) ) {
          
          ################################################################
          #RED edges here
          ################################################################
          
          BT2DTAdj[nBT,i] <<- 1
          BT2DTRedEdge[nBT,i] <<- 1
          
          BT2DTlinesMTX <<- rbind(BT2DTlinesMTX, DTMtx[i,]) 
          BT2DTlinesMTX <<- rbind(BT2DTlinesMTX, BTMtx[nBT,])
          
          BT2DTlinesBuffer <- rbind(BT2DTlinesBuffer, c(DTMtx[i,],"red")) 
          BT2DTlinesBuffer <- rbind(BT2DTlinesBuffer, c(BTMtx[nBT,],"red"))
          
          
          #update edge weights too
          #BT2DTedgeW[i,j] <- BT2DTMtx[i,j]
          BT2DTedgeW[nBT,i] <<- BT2DTMtx[nBT,i]
          
          
          
          #DEBUG
          cat("RED Reachable: Connecting vertex", nVertex, "Boat no", nBT ,"to depot", i,"dist=",BT2DTedgeW[nBT,i],"\n")
          gRedGray <<- gRedGray             %>% add_edges(c(nVertex,i), color=redEdgeCol, width = 1, weight = BT2DTedgeW[nBT,i])
          gRedGrayYellow <<- gRedGrayYellow %>% add_edges(c(nVertex,i), color=redEdgeCol, width = 1, weight = BT2DTedgeW[nBT,i])
          
          if(input$edgeFilter=="gr"){
            myGraph$value <- gGray
          }else if(input$edgeFilter=="rg"){
            myGraph$value <- gRedGray
          }else if(input$edgeFilter=="rgy"){
            myGraph$value <- gRedGrayYellow
          }
          
        }# if for red edge
        
      }else{
        
        cat("No GRAY edge for", nVertex, "Boat no", nBT,"\n")
      }# else no gray edge
      
    } #end of for loop over the depots
    
    
    
    
    
    
    
    
    
    
    
    #######################################         
    #END loops for between depots and boats
    #######################################   
    
    
    # 
    # 
    # 
    # ###############################################################
    # #Red pruning starts here
    # ###############################################################
    # 
    # ###############################################################
    # #Lets prune the red edges that can not be paired with the
    # #gray edges here
    # #Then the sp algo will be able to check less
    # ###############################################################
    # currentDroneRange <- reac()$drone_range
    # rescueDist <<-  ceiling(currentDroneRange / 2)
    # 
    # edgeList <- incident(gRedGray, nVertex)
    # nEdge <- length(edgeList)
    # neighborList <- neighbors(gRedGray, nVertex)
    # nNeighbor <- length(neighborList)
    # 
    # minGrayEdgeWeight <- Inf
    # minGrayEdgeSrcVtxId <- -1 #The dest vertex is the Boat!!!
    # 
    # 
    # #NOTE for edgeList:
    # #ends(gRedGray, edgeList[k])[1] gives the src[1] id of the kth edge
    # #ends(gRedGray, edgeList[k])[2] gives the dest[1] id of the kth edge
    # foundGray <- 0 
    # for (k in (1:nEdge)) {
    #   
    #   #DEBUG
    #   cat("**** Adding boat ****Boat",nBT,"has total",nEdge,"incident edges. Incident edge" ,k,
    #       "from vtx",ends(gRedGray, edgeList[k])[1] ,"with weight",edgeList[k]$weight,
    #       "color", edgeList[k]$color,
    #       "rescuedist",rescueDist,"\n")
    #   if (edgeList[k]$weight <= rescueDist) {
    #     foundGray <- 1
    #     #break
    #     if (edgeList[k]$weight < minGrayEdgeWeight){
    #       minGrayEdgeWeight <- edgeList[k]$weight
    #       minGrayEdgeSrcVtxId <- ends(gRedGray, edgeList[k])[1]
    #     }
    #     
    #   }
    #   
    # } #end for
    # 
    # #Now that we found the min cost gray edge (if any)
    # #Lets prune the unusable red edges
    # #edges with cost that when added to the  minGrayEdgeWeight 
    # #the sum will be greater than the drone range
    # #we assume that we have depot after that!!!!
    # if (foundGray == 1) {
    #   #we have gray edge we can prune unusable red edges
    #   for (k in (1:nEdge)) {
    #     if ( (edgeList[k]$color == redEdgeCol) && ( (edgeList[k]$weight + minGrayEdgeWeight) > currentDroneRange) ){
    #       #Remove the red edge from the gRedGray
    #       
    #       depotVtxNo <- as.numeric(ends(gRedGray, edgeList[k])[1])
    #       #DEBUG
    #       cat("**** Adding boat ****Removing red edge",k ,"from",ends(gRedGray,edgeList[k])[1],"to",ends(gRedGray,edgeList[k])[2],
    #           "with gray:",(edgeList[k]$weight + minGrayEdgeWeight),"drange:",currentDroneRange,"\n")
    #       gRedGray <<- delete.edges(gRedGray, E(gRedGray, c(depotVtxNo, nVertex)))
    #     }
    #     
    #   }#end for over edges
    #   
    # } else {
    #   #no gray edge prune all the red if any!!!
    #   for (k in (1:nEdge)) {
    #     if (edgeList[k]$color == redEdgeCol){
    #       #Remove the red edge from the gRedGray
    #       depotVtxNo <- as.numeric(ends(gRedGray, edgeList[k])[1])
    #       gRedGray <<- delete.edges(gRedGray, E(gRedGray, c(depotVtxNo, nVertex)))
    #     }
    #     
    #   }#end for over edges
    #   
    # }
    # 
    # 
    # ###############################################################
    # #Red pruning ends here
    # ###############################################################
    # 
    
    
    
    
    
    
    
    ######################################################### 
    #BEGIN loop for among boats. IF YOU HAVE AT LEAST 2 BOATS
    ######################################################### 
    
    #Check the current boat "nBT" against all the others
    if (nBT >= 2) {
      for (i in (1:(nBT-1))){
        
        #Find the dist in meters between two boats
        BT2BTMtx[nBT, i] <<- lonlat2m(BTMtx[i,1], BTMtx[i,2], BTMtx[nBT,1], BTMtx[nBT,2]) #lon1,lat1,lon2,lat2
        
        
        #DEBUG
        cat("****> Boat:", nBT,"and boat:" ,i,"coords",BTMtx[i,],"\n")
        
        #DEBUG if (i==1) cat("Dist between ",i, "at (",dx1,dy1,") and",j, "is",dist,"\n")
        #DEBUG cat ("Dist = ",dist,"\n")
        
        #Connect the boat to boat that can be reached
        yellowDist <<- floor(currentDroneRange / 2)
        if (BT2BTMtx[nBT, i] <= yellowDist) {
          #BT2DTAdj[i,j] <- 1
          BT2BTAdj[nBT,i] <<- 1
          
          BT2BTlinesMTX <<- rbind(BT2BTlinesMTX, BTMtx[i,]) 
          BT2BTlinesMTX <<- rbind(BT2BTlinesMTX, BTMtx[nBT,])
          
          BT2BTlinesBuffer <- rbind(BT2BTlinesBuffer, BTMtx[i,]) 
          BT2BTlinesBuffer <- rbind(BT2BTlinesBuffer, BTMtx[nBT,])
          
          
          #update edge weights too
          #BT2DTedgeW[i,j] <- BT2DTMtx[i,j]
          BT2BTedgeW[nBT,i] <<- BT2BTMtx[nBT,i]
          
          
          
          #DEBUG
          cat("YELLOW Reachable Boats: Connecting boat", i, "to boat no", nBT ,"\n")
          gRedGrayYellow <<- gRedGrayYellow %>% add_edges(c(nVertex, (nBS + nDT + i)), color=yellowEdgeCol, width = 1, weight = BT2BTedgeW[nBT,i])
          #Maybe gWithoutYellow?
          #gWithoutRed <<- gWithoutRed %>% add_edges(c(nVertex,i), color=grayEdgeCol, width = 1, weight = BT2DTedgeW[nBT,i])
          
          if(input$edgeFilter=="gr"){
            myGraph$value <- gGray
          }else if(input$edgeFilter=="rg"){
            myGraph$value <- gRedGray
          }else if(input$edgeFilter=="rgy"){
            myGraph$value <- gRedGrayYellow
          }
        } else if(BT2BTMtx[nBT, i] <= currentDroneRange) {
          
          # For the time being lets do nothing for this case
          
          # BT2DTAdj[nBT,i] <<- 1
          # 
          # BT2DTlinesMTX <<- rbind(BT2DTlinesMTX, DTMtx[i,]) 
          # BT2DTlinesMTX <<- rbind(BT2DTlinesMTX, BTMtx[nBT,])
          # 
          # BT2DTlinesBuffer <- rbind(BT2DTlinesBuffer, DTMtx[i,]) 
          # BT2DTlinesBuffer <- rbind(BT2DTlinesBuffer, BTMtx[nBT,])
          # 
          # 
          # #update edge weights too
          # #BT2DTedgeW[i,j] <- BT2DTMtx[i,j]
          # BT2DTedgeW[nBT,i] <<- BT2DTMtx[nBT,i]
          # 
          # 
          # 
          # #DEBUG
          # cat("RED Reachable: Connecting vertex", nVertex, "Boat no", nBT ,"to depot", i,"\n")
          # gRedGray <<- gRedGray %>% add_edges(c(nVertex,i), color=redEdgeCol, width = 1, weight = BT2DTedgeW[nBT,i])
          # myGraph$value <- gRedGray
          
          
        } #if
      }
    }else{
      cat("Single Boat\n")
    }
    
    
    ####################################### 
    #END loop for among boats
    ####################################### 
    
    ####################################################################  
    #Update the graph and the plot
    ####################################################################      
    # We need lines between depots and boats
    ####################################################################
    
    adjSum <- sum(BT2DTAdj[nBT,])
    if (adjSum == 0) {
      cat("Boat",nBT, "is not reachable!\n")
      #DEBUG
      cat("Added isolated vertex:", nVertex,"with label",paste0("B",nBT), "\n")
      #gRedGray <<- gRedGray %>% add_vertices(1, color="blue", label = paste0("B",nBT), label.cex = 0.3) 
      #myGraph$value <- gRedGray
      #%>% add_edges(c(nVertex,i, i,nVertex), color=grayEdgeCol, width = 1, weight = Inf)
    }else{
      cat("Boat",nBT, "is reachable from",adjSum,"depots!\n")
    }
    
    #Draw all the lines for "adjacent" depots
    if (adjSum > 1){
      for (k in seq(1, (2*adjSum-1), by = 2)) {
        df <- as.data.frame(rbind(BT2DTlinesBuffer[k,], BT2DTlinesBuffer[k+1,]))
        colnames(df) <- c("x","y", "edgeColor")
        df$x <- as.numeric(df$x)
        df$y <- as.numeric(df$y)
        
        leafletProxy("mymap") %>% 
          addPolylines(data = df,
                       lng = ~ x, 
                       lat = ~ y,
                       color = ~ edgeColor,
                       weight = 2,
                       group = "BT2DTEdges")
        
        
        mapToSave$current <- mapToSave$base %>% 
          addPolylines(data = df,
                       lng = ~ x, 
                       lat = ~ y,
                       color = ~ edgeColor,
                       weight = 2,
                       group = "BT2DTEdges")
      }
      
      
    }else if (adjSum == 1){
      df <- as.data.frame(rbind(BT2DTlinesBuffer[1,], BT2DTlinesBuffer[2,]))
      colnames(df) <- c("x","y", "edgeColor")
      df$x <- as.numeric(df$x)
      df$y <- as.numeric(df$y)
      
      leafletProxy("mymap") %>% 
        addPolylines(data = df,
                     lng = ~ x, 
                     lat = ~ y,
                     color = ~ edgeColor,
                     weight = 2,
                     group = "BT2DTEdges")
      
      
      mapToSave$current <- mapToSave$base %>% 
        addPolylines(data = df,
                     lng = ~ x, 
                     lat = ~ y,
                     color = ~ edgeColor,
                     weight = 2,
                     group = "BT2DTEdges")
    }else{
      cat("No lines from depots. Boat",nBT, "is not reachable!\n")
    }
    
    
    ####################################################################   
    
    
  }
  
  ########################################################################################################
  
  
  ######################################################################################################## 
  #Add boats to the active depot map
  addAndShowBoatsActive <- function() {
    
    cat("Drawing Boats1\n")
    if(activeGridType == "tri"){
      
      BTMtx <<- TriBTMtx
      VTXMtx <<- TriVTXMtx
      nBT <<- TrinBT
      
      #Active
      gBSandDT <<- gTriBSandDT
      gRedGray <<- gTriRedGray
      gGray  <<- gTriGray 
      gRedGrayYellow  <<-  gTriRedGrayYellow
      
      
      
      
      if(input$edgeFilter=="gr"){
        myGraph$value <- gTriGray
      }else if(input$edgeFilter=="rg"){
        myGraph$value <- gTriRedGray
      }else if(input$edgeFilter=="rgy"){
        myGraph$value <- gTriRedGrayYellow
      }
      
      cat("Tri is active. Drawing", length(E(myGraph$value)), "edges.\n")
      
    }else  if(activeGridType == "sq"){
      
      BTMtx <<- SqBTMtx
      VTXMtx <<- SqVTXMtx
      nBT <<- SqnBT
      
      #Active
      gBSandDT <<- gSqBSandDT
      gRedGray <<- gSqRedGray
      gGray  <<- gSqGray 
      gRedGrayYellow  <<-  gSqRedGrayYellow
      
      
      if(input$edgeFilter=="gr"){
        myGraph$value <- gSqGray
      }else if(input$edgeFilter=="rg"){
        myGraph$value <- gSqRedGray
      }else if(input$edgeFilter=="rgy"){
        myGraph$value <- gSqRedGrayYellow
      }
      
      cat("Sq is active. Drawing", length(E(myGraph$value)), "edges.\n")
    }
    
    cat("Drawing Boats2\n")
    
    
    
    
    #Show boats
    isolate({
      new_zoom <- 9
      if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
      
      ##Icons for boats. Also mark the pos with black circles
      
      df <- as.data.frame(BTMtx)
      colnames(df) <- c("x","y")
      
      leafletProxy("mymap") %>% 
        
        addCircles(data = df,
                   lng = ~ x, 
                   lat = ~ y,
                   color = "black",
                   radius = 10,
                   opacity = 5,
                   group = "boatCircles" )%>% 
        addMarkers(data = df,
                   lng = ~ x, 
                   lat = ~ y,
                   icon = myBTIcon)  
      
      
      mapToSave$current <- mapToSave$base %>% 
        addCircles(data = df,
                   lng = ~ x, 
                   lat = ~ y,
                   color = "black",
                   radius = 10,
                   opacity = 5,
                   group = "boatCircles")%>% 
        addMarkers(data = df,
                   lng = ~ x, 
                   lat = ~ y,
                   icon = myBTIcon)  
    })
  }
  
  ########################################################################################################
  
  
  
  
  
  
  
  ########################################################################################################
  #Saves boat coordinates to a file
  saveBoatPos <- function(boatPos) {
    
    boatPosCNT <<- boatPosCNT + 1
    boatPosTXT <<- str_pad(boatPosCNT, 4, pad = "0")
    boatPosFileName <<- paste0("boatPos-",boatPosTXT,".csv")
    boatPosFile <<- paste(myDataDir,boatPosFileName, sep = "/")
    cat("Saving boat configuration to", boatPosFile,"\n")
    
    nboat <- nrow(boatPos)
    btID <- 1:nboat
    boatPosDF <- cbind(btID, boatPos)
    colnames(boatPosDF) <- c("bid","lng","lat")
    write_excel_csv(boatPosDF, boatPosFile)
    
    
    
    
    
  }
  
  ########################################################################################################
  
  
  
  ########################################################################################################
  #Recreate boats every time button is pressed
  createRNDBoats <- function(){ 
    
    
    
    #Enable the method buttons since we have not to save!!!
    enable("rescueRedGray1by1")
    enable("rescueGray1by1")
    enable("permRedGray")
    enable("permGray")
    enable("clookRedGray")
    enable("clookGray")
    enable("gaPermRedGray")
    enable("gaSectorRedGray")
    
    
    
    if (nBT>0) {
      
      cat("Re-Deploying Depots for new set of Boats\n")
      resetForNewBoatSet()
      setButtonsAfterDeployDepots()
      
      
      if(activeGridType == "tri"){
        if (TriDTReady == FALSE) prepareTriDepotPos()
        activateDepots("tri")
        
      }else  if(activeGridType == "sq"){
        
        if (SqDTReady == FALSE) prepareSqDepotPos()
        activateDepots("sq")
      }
      
      cat("Re-Drawing Depots for new set of Boats\n")
      drawActiveDepots()
      cat("Re-Drawing Depots for new set of Boats done\n")
    }
    
    
    
    #DEBUG
    cat("Recreating",input$rnd_boat_number,"boats!\n")
    #nBT <<- input$rnd_boat_number
    nRNDBT <- input$rnd_boat_number
    
    #Create RND Boat positions
    
    #First create a smaller polygon so that all the boats will be "reachable"
    regionPolyScaled <- rgeos::gBuffer(regionPolyDF,width=-0.1)
    
    
    #boatPosSample <- spsample(as(regionPolyScaled,"Spatial"), input$rnd_boat_number, type = "random" )
    
    #boatPosSample <- spsample(as(regionPoly,"Spatial"), input$rnd_boat_number, type = "random" )
    
    boatPosSample <- spsample(regionPolyScaled, input$rnd_boat_number, type = "random" )
    
    
    boatPos <- as.data.frame(boatPosSample@coords) 
    colnames(boatPos) <- c("x","y")
    
    #Some parts of the region is not covered
    #Boats in these parts can not be reached!!!
    #Put barrier for all reachable boats!!!
    while (checkIfAllBoatsReachableForGrids(boatPos) == FALSE){
      boatPosSample <- spsample(as(regionPoly,"Spatial"), input$rnd_boat_number, type = "random" )
      boatPos <- as.data.frame(boatPosSample@coords) 
      colnames(boatPos) <- c("x","y")
    }
    
    #Check if they are in polygon!!
    for (btIDX in 1:nRNDBT) {
      if (point.in.polygon(boatPos[btIDX,]$x, boatPos[btIDX,]$y, regionPoly[[1]][,1], regionPoly[[1]][,2]) == 0) {
        cat("Boat pos",btIDX,"at",boatPos[btIDX,]$x, boatPos[btIDX,]$y,"is outside of region!\n")
      } else {
        
        
        #Add the boat to the grids
        addBoatTri(boatPos, btIDX)
        addBoatSq(boatPos, btIDX)
        
        
        
        btLng <- boatPos[btIDX,]$x
        btLat <- boatPos[btIDX,]$y
        
        
        cat("Boat", btIDX, "added. Drawing edges.\n")
        
        
        
      }#else
      
      
      
    }#end for loop for boats
    
    
    
    
    
    addAndShowBoatsActive()
    
    saveBoatPos(boatPos)
    
    
    #DEBUG
    cat(nBT,"boats created!\n")
    cat(nBSandDepot,"BS and Depot!\n")
    cat(nVertex, "vertices!\n")
  }
  
  ########################################################################################################  
  
  
  
  ########################################################################################################
  #Recreate boats every time button is pressed
  observeEvent(input$rndBoats,{
    createRNDBoats()
  })
  
  ########################################################################################################
  
  
  
  
  ########################################################################################################
  #Runs sims nSim times
  observeEvent(input$runSim, {
    
    numSim <- input$nSim
    
    enable("rndBoats")
    enable("rnd_boat_number")
    
    if (TriDTReady == FALSE) prepareTriDepotPos()
    if (SqDTReady == FALSE) prepareSqDepotPos()
    
    for (s in 1:numSim) {
      
      cat("***********> Running sim", s, "\n")
      tic()
      
      createRNDBoats()
      
      
      #For Tri grid type
      activateDepots("tri")
      
      #DEBUG uncomment for bruteforce
      #permAlgo("rg")
      #permAlgo("g")

      clookAlgo("rg")
      clookAlgo("g")
      concaveTSPAlgo("rg")
      concaveTSPAlgo("g")
      
      for(method in xTSPmethods){
        xTSPAlgo("rg", method)
        xTSPAlgo("g", method)
      }
      
      
      #For Sq grid type
      activateDepots("sq")
      
      #DEBUG uncomment for bruteforce
      #permAlgo("rg")
      #permAlgo("g")
      
      
      clookAlgo("rg")
      clookAlgo("g")
      concaveTSPAlgo("rg")
      concaveTSPAlgo("g")
      
      for(method in xTSPmethods){
        xTSPAlgo("rg", method)
        xTSPAlgo("g", method)
      }
      
      
      simElapsed <- toc()
      cat("***********> Sim", s, "took", (simElapsed$toc - simElapsed$tic), "secs.\n\n")
      
    }
    
    
  })
  
  ########################################################################################################
  
  
  
  
  
  
  
  
  
  
  
  ########################################################################################################
  output$mouseCoord <- renderText({
    if(is.null(input$hover_coordinates)) {
      "Mouse outside of map"
    } else {
      paste0("Lat: ", round(input$hover_coordinates[1],nPrecDigit), 
             "\nLng: ", round(input$hover_coordinates[2],nPrecDigit))
    }
  })
  ########################################################################################################
  
  
  
  
  
  ########################################################################################################
  
  ####################################################################
  #make dynamic slider
  output$depot_spacing <- renderUI({
    #To make sure depots are spaced justly for connection
    #depot_spacing <- floor(input$drone_range / sqrt(2))
    
    #Max distfrom any depor or from BS that a boat can be rescued
    #rescueDist$val <- floor(input$drone_range / 2)
    
    
    #Make distances little bit less than the range with delta
    #delta$val <- ceiling(0.05 * input$drone_range)
    
    
    ui_text <- paste0("Depot Spacing (drone range= ",as.character(input$drone_range), "):")
    sliderInput("depot_spacing", 
                ui_text, 
                min=floor(input$drone_range / 3), 
                max=input$drone_range, 
                step = 250,
                value=floor(input$drone_range / sqrt(2)))
    
    #For the time being disable it!!!
    disable("depot_spacing")
  }) #end of output$depot_spacing
  ####################################################################
  ########################################################################################################
  
  
  
  ####################################################################
  #Here list the sliders that can effect redraw of the plots
  #with debounce_sc add milisecs of delay after the update of the listed var for redraw
  
  reac <- reactive(list(bsx = input$bsx, 
                        bsy  = input$bsy, 
                        drone_range = input$drone_range, 
                        #myGraphScale = input$myGraphScale,
                        #depot_spacing =input$depot_spacing, 
                        nBoat = input$nBoat)) %>% debounce_sc(100)
  
  #debounce_sc(100, short_circuit = reactive(input$redraw))
  
  ####################################################################
  
  
  
  
  
  
  
  
  
  
  ########################################################################################################
  observeEvent(input$clearSelectionRegion,{
    #DEBUG
    cat("*** Reset all!\n")
    #Disable startBS and startBT
    updateSwitchInput(
      session = session,
      inputId = "startBS",
      disabled = TRUE,
      value = FALSE,
    )
    
    
    updateSwitchInput(
      session = session,
      inputId = "startBT",
      disabled = TRUE,
      value = FALSE,
    )
    
    
    #Enable startPoly for new region but let user to switch it on
    updateSwitchInput(
      session = session,
      inputId = "startPoly",
      disabled = FALSE, 
      value = FALSE,
    )
    
    
    updateRadioButtons(
      session = session, 
      inputId = "grid",
      selected = "tri"
    )
    
    activeGridType <<- "tri"
    
    
    updateRadioButtons(
      session = session, 
      inputId = "edgeFilter",
      selected = "rg"
    )
    
    
    disable("rndBoats")
    disable("rnd_boat_number")
    disable("closePolygon")
    
    disable("deployTriDepots")
    disable("deploySqDepots")
    
    disable("rescueRedGray1by1")
    disable("rescueGray1by1")
    disable("permRedGray")
    disable("permGray")
    disable("clookRedGray")
    disable("clookGray")
    disable("gaPermRedGray")
    disable("gaSectorRedGray")
    
    my_reset_all_globals()
    
    isolate({
      new_zoom <- 9
      if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
      output$myMsg <- renderText({paste("Cleared Selection: Lat:", round(input$mymap_center$lat,nPrecDigit),"Lng:", round(input$mymap_center$lng,nPrecDigit),"Zoom:",new_zoom, "\n")})
      output$clickMsg<-renderText({ paste("Cleared Selection.\n") })
      leafletProxy("mymap") %>%  clearMarkers() %>% clearShapes()
      
      #Like this update also the map for saving
      mapToSave$current <- mapToSave$base %>%  clearMarkers() %>% clearShapes()
      
      #The graph pf the depot config
      myGraph$value <- gRedGray
    })
  })
  ########################################################################################################
  
  
  
  
  
  
  
  
  
  ########################################################################################################
  #BEGIN MY ALGO
  ########################################################################################################
  
  
  
  
  
  
  
  
  ########################################################################################################
  #BEGIN rescueGray1by1
  ########################################################################################################
  
  ########################################################################################################
  #OK uses gTemp
  #OK infotext prints the method name
  #No AWD analysis is necessary
  #This algorithm sends a single drone for each boat uses Red-Gray heuristics
  observeEvent(input$rescueGray1by1,{
    tic("rescueGray1by1")
    #Boats should be added to mtx when the user clicks
    #Here just find the sp and plot it!!!
    
    cat("Rescuing - Gray1by1", nBT ,"boats with",nBSandDepot,"bs+depots.\n")
    
    
    #Assuming that we have the graph save local copy to work on it
    gTemp <- gGray
    updateRadioButtons(
      session = session, 
      inputId = "edgeFilter",
      selected = "g"
    )
    
    currentDroneRange <- reac()$drone_range
    my_text <- paste("Single Boat per Drone Rescue - Gray. Drone range =",reac()$drone_range, "meters.\n")
    
    totRescueDist <- 0
    ################################################
    #CC Analysis of the graph. Good for multiple BSs
    #In fact for multi BS case you check for each boat
    #If there is any boat that it is not in any CC with any BS
    #Then this boat is helpless.
    #So check orphan boats!!!
    ################################################
    #Here check if depots are connected or isolated label them!!!
    cl <- components(gTemp)
    #DEBUG
    cat("There are", cl$no, "CCs in the graph\n");
    cclist <- lapply(seq_along(cl$csize)[cl$csize > 0], function(x) V(gTemp)$label[cl$membership %in% x])
    
    #Lets find the group BS is in!!!
    BSccId <- which("BS1" %in% V(gTemp)$label[cl$membership])
    #May be BSccId <- cl$membership[1]
    
    ################################################
    #DEBUG
    # cat("CClist:", unlist(cclist), "\n")
    cat("CCs are: ")
    for (i in (1:length(cclist))) {
      cat(cclist[[i]], "-- ")
    }
    cat("\n")
    ################################################
    
    
    ################################################
    #For each boat find the sp from the BS or BSs
    ################################################
    
    
    #We need an array or a vector to store the rescuePaths for each Boat!!!
    #Later we will check the intersections and stuff
    
    rescuePathList <- list()
    for(i in (1:nBT)){
      
      BTlabel <- paste0("B",i)
      BTvtxId <- nBSandDepot + i
      #First check if the boat and BS are in the same CC
      BTccId <- cl$membership[BTvtxId]
      
      if (BTccId == BSccId){
        #DEBUG
        cat("Boat",i,"is in the same CC with BS.\n")
        #Now check if the all edges are red
        #If there is no gray edge than the boat can not be saved
        #THIS IS WITHOUT CONSIDERING THE BOAT CHAIN!!!
        
        
        
        edgeList <- incident(gTemp, BTvtxId)
        nEdge <- length(edgeList)
        neighborList <- neighbors(gTemp, BTvtxId)
        nNeighbor <- length(neighborList)
        
        minGrayEdgeWeight <- Inf
        minGrayEdgeSrcVtxId <- -1 #The dest vertex is the Boat!!!
        
        foundGray <-0 
        for (k in (1:nEdge)) {
          
          #DEBUG
          cat("Boat",i,"has total",nEdge,"incident edges. Incident edge" ,k,
              "from vtx",ends(gTemp, edgeList[k])[1] ,"with weight",edgeList[k]$weight,
              "color", edgeList[k]$color,
              "rescuedist",rescueDist,"\n")
          if (edgeList[k]$weight <= rescueDist) {
            foundGray <- 1
            #break
            if (edgeList[k]$weight < minGrayEdgeWeight){
              minGrayEdgeWeight <- edgeList[k]$weight
              minGrayEdgeSrcVtxId <- ends(gTemp, edgeList[k])[1]
            }
            
          }
          
        }
        
        if (foundGray == 1) {
          #Weighted and unweighted shortest paths from BS=1 to the boat
          wSPs <- get.shortest.paths(gTemp, 1, BTvtxId)$vpath
          uwSPs <- get.shortest.paths(gTemp, 1, BTvtxId, weights=NA)$vpath 
          #get the index of the first path that has min number of hops
          minHopPathIdx <- which.min(sapply(wSPs,length))
          wSP <- unlist(wSPs[[minHopPathIdx]])
          
          #The last edge of the wSP can be a red edge (> rescueDist)!!!
          #This is possible if there is a gray path and the total
          #of the gray+red path is less than or equal(?) to the drone range
          #Otherwise the gray path should be selected!!!
          
          #Check if the sp uses red edge
          lastVTX <- wSP[length(wSP)-1]
          
          #Color of the last edge. BUT CAN BE CHANGED BEFORE THE CHECK!!!!
          #E(gTemp, path=wSP)[length(wSP)-1]$color
          #Weight of the last edge. STABLE ALWAYS!!!
          #E(gTemp, path=wSP)[length(wSP)-1]$weight
          
          
          if (E(gTemp, path=wSP)[length(wSP)-1]$color == "red"){
            #We are using red edge for the sp
            #Now we need to check if there is a gray edge to return to the depot network
            #DEBUG
            cat("wSP uses red edge from vtx", lastVTX, "to boat", i,"\n")
            cat("Searching for gray return edge if total of gray+red <= droneRange.\n")
            cat("Min gray edge has weight:", minGrayEdgeWeight, "and the src vtx is", minGrayEdgeSrcVtxId,".\n")
            totalPathLentoReturn <- minGrayEdgeWeight + E(gTemp, path=wSP)[length(wSP)-1]$weight
            if (totalPathLentoReturn <= input$drone_range) {
              cat("There is a return path via gray vtx",minGrayEdgeSrcVtxId,"total weight=", 
                  totalPathLentoReturn, "drone range", input$drone_range,"\n")
              E(gTemp, c(BTvtxId,minGrayEdgeSrcVtxId))$color <- rescuePathCol
              E(gTemp, c(BTvtxId,minGrayEdgeSrcVtxId))$width <- 1
            }else{
              cat("No return path via gray vtx",minGrayEdgeSrcVtxId,"total weight=", totalPathLentoReturn, "drone range", input$drone_range,"\n")
              #Here generate graph "f" by removing the red edge from the graph "gTemp"
              #or you can set the weight to Inf on f
              #Try the sp again and test stuff again
              #Till you find a better path
              #If this was the only red
              
              #Remove the red edge used in sp from the gTemp
              vno <- as.numeric(lastVTX)
              gTemp <- delete.edges(gTemp, E(gTemp, c(vno, BTvtxId)))
              #DEBUG
              cat("Red edge",vno,"to",BTvtxId,"( Boat",i,") is removed from the graph.\n")
              #ftemp <- f
              #Find new sp on f
              #Check the same conditions
              #Continue till you find a "returnable" sp!!!
              #Remember we have at least single gray edge
              FoundgoodSP <- 0
              
              while(FoundgoodSP == 0){
                
                #Weighted and unweighted shortest paths from BS=1 to the boat
                wSPs <- get.shortest.paths(gTemp, 1, BTvtxId)$vpath
                uwSPs <- get.shortest.paths(gTemp, 1, BTvtxId, weights=NA)$vpath 
                #get the index of the first path that has min number of hops
                minHopPathIdx <- which.min(sapply(wSPs,length))
                wSP <- unlist(wSPs[[minHopPathIdx]])
                
                #The last edge of the wSP can be a red edge (> rescueDist)!!!
                #This is possible if there is a gray path and the total
                #of the gray+red path is less than the drone range
                #Otherwise the gray path should be selected!!!
                
                #Check if the sp uses red edge
                lastVTX <- wSP[length(wSP)-1]
                
                
                if (E(gTemp, path=wSP)[length(wSP)-1]$color == "red"){
                  #We are using red edge for the sp
                  #Now we need to check if there is a gray edge to return to the depot network
                  #DEBUG
                  cat("wSP uses red edge from vtx", lastVTX, "to boat", i,"\n")
                  cat("Searching for gray return edge if total of gray+red <= droneRange.\n")
                  cat("Min gray edge has weight:", minGrayEdgeWeight, "and the src vtx is", minGrayEdgeSrcVtxId,".\n")
                  totalPathLentoReturn <- minGrayEdgeWeight + E(gTemp, path=wSP)[length(wSP)-1]$weight
                  if (totalPathLentoReturn <= input$drone_range) {
                    cat("***There is a return path via gray vtx",minGrayEdgeSrcVtxId,"total weight=", 
                        totalPathLentoReturn, "drone range", input$drone_range,"\n")
                    E(gTemp, c(BTvtxId,minGrayEdgeSrcVtxId))$color <- rescuePathCol
                    E(gTemp, c(BTvtxId,minGrayEdgeSrcVtxId))$width <- 2
                    FoundgoodSP <- 1
                  }else{
                    cat("No return path via gray vtx",minGrayEdgeSrcVtxId,"total weight=", totalPathLentoReturn, "drone range", input$drone_range,"\n")
                    FoundgoodSP <- 0
                    #REmove the red edge used in sp from the gTemp
                    vno <- as.numeric(lastVTX)
                    gTemp <- delete.edges(gTemp, E(gTemp, c(vno, BTvtxId)))
                    #f <- ftemp
                    
                  }#else
                  
                }else{
                  cat ("*** From rescueGray1by1 *** wSP uses gray edge from vtx", lastVTX, "to boat", i,"\n")  
                  FoundgoodSP <- 1
                }
              }#while
            }
          }else{
            cat ("*** From rescueGray1by1 *** wSP uses gray edge from vtx", lastVTX, "to boat", i,"\n")  
            
          }
          
          
          
          #Unweighted paths they all have the same hops!!!!
          #So just take the first one!!!
          uwSP <- unlist(uwSPs[[1]])
          #If the boat is directly reachable from BS gives error!
          #"Error in E: At type_indexededgelist.c:1173 : Cannot get edge id, no such edge, Invalid value"
          
          #E(gTemp)$color <<- "green"
          E(gTemp, path=wSP)$color <- rescuePathCol
          E(gTemp, path=wSP)$width <- 2
          
          #DEBUG
          #E(gTemp, path=uwSP)$color <<- yellowEdgeCol
          #E(gTemp, path=uwSP)$width <<- 3
          
          
          #Finally top plot the changes on the local copy of the graph assign it to reactive myGraph
          myGraph$value <- gTemp
          
          #plot(gTemp, vertex.size=15, edge.label = E(gTemp)$weight, layout=layout_with_graphopt(gTemp))
          #plot(gTemp, vertex.size=15, edge.label = E(gTemp)$weight, layout=myCoords)
          
          spHops <- length(wSP) - 1
          spDist <- sum(E(gTemp, path=unlist(wSP))$weight)
          
          rescuePathList[[i]] <- wSP
          
          if (spHops == 1) {
            my_text <- paste(my_text,"Boat",i,"Shortest rescue path directly from BS! Hops:",spHops,"Distance:",round(spDist,nPrecDigit),"meters.\n")
            totRescueDist <- totRescueDist + spDist
          } else {
            minPathStr <- paste(wSP$label,collapse = " -> ")
            my_text <- paste(my_text,"Boat",i,"Shortest rescue path from BS",spHops,"hops with distance (drone range) =",round(spDist/input$drone_range,nPrecDigit),":", minPathStr,"\n")
            totRescueDist <- totRescueDist + spDist
          }
          
          cat(my_text)
          output$msg1 <- renderText({my_text})
          
        }else{
          #DEBUG
          my_text <- paste(my_text,"Boat",i,"has no gray rescue edge.\n")
          cat(my_text)
          output$msg1 <- renderText({my_text})
          rescuePathList[[i]] <- -1
        }
        
        
      }else{
        #DEBUG
        my_text <- paste(my_text,"Boat",i,"can not be saved. It is not in the same CC with BS.\n")
        cat(my_text)
        output$msg1 <- renderText({my_text})
        rescuePathList[[i]] <- -1
      }
      
    }#for
    
    
    my_text <- paste(my_text,
                     "----------------------------------------\n",
                     "Total rescue path necessary for",nBT,"boats:", round(totRescueDist,nPrecDigit),"m",round(totRescueDist/input$drone_range,nPrecDigit),"drone range\n")
    output$msg1 <- renderText({my_text})
    ################################################
    #Do some rescuePath analysis
    ################################################
    
    if (nBT > 1){
      cat("analyzing for", nBT,"boats rescue paths.\n")
      for (i in (1:(nBT-1))){
        for (j in ((i+1):nBT)){
          cat("Boat",i,"and",j,"analysis:\n")
          #What about crossing edges: Edges with single common vertex?
          if (rescuePathList[[i]][1] != -1  && rescuePathList[[j]][1] != -1 ){
            PathIntersect <- intersection(rescuePathList[[i]], rescuePathList[[j]])
            cat("Intersect:",PathIntersect,"\n")
          }else{
            cat("One boat can not be saved\n")
          }
        }
      }
    }else{
      cat("Just a single boat no need rescue path analysis.\n")
    }
    
    
    ################################################
    
    
    isolate({
      new_zoom <- 9
      if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
    })
    cat("Rescue Lines are drawn.\n")
    toc()
  })
  ########################################################################################################
  
  
  
  
  
  
  ########################################################################################################
  #OK uses gTemp
  #OK infotext prints the method name
  #No AWD analysis is necessary
  #This algorithm sends a single drone for each boat uses Red-Gray heuristics
  observeEvent(input$rescueRedGray1by1,{
    tic("rescueRedGray1by1")
    #Boats should be added to mtx when the user clicks
    #Here just find the sp and plot it!!!
    
    cat("Rescuing - RedGray1by1", nBT ,"boats with",nBSandDepot,"bs+depots.\n")
    
    
    #Assuming that we have the graph save local copy to work on it
    gTemp <- gRedGray
    updateRadioButtons(
      session = session, 
      inputId = "edgeFilter",
      selected = "rg"
    )
    
    currentDroneRange <- reac()$drone_range
    my_text <- paste("Single Boat per Drone Rescue - Red-Gray. Drone range =",reac()$drone_range, "meters.\n")
    
    totRescueDist <- 0
    ################################################
    #CC Analysis of the graph. Good for multiple BSs
    #In fact for multi BS case you check for each boat
    #If there is any boat that it is not in any CC with any BS
    #Then this boat is helpless.
    #So check orphan boats!!!
    ################################################
    #Here check if depots are connected or isolated label them!!!
    cl <- components(gTemp)
    #DEBUG
    cat("There are", cl$no, "CCs in the graph\n");
    cclist <- lapply(seq_along(cl$csize)[cl$csize > 0], function(x) V(gTemp)$label[cl$membership %in% x])
    
    #Lets find the group BS is in!!!
    BSccId <- which("BS1" %in% V(gTemp)$label[cl$membership])
    #May be BSccId <- cl$membership[1]
    
    ################################################
    #DEBUG
    # cat("CClist:", unlist(cclist), "\n")
    cat("CCs are: ")
    for (i in (1:length(cclist))) {
      cat(cclist[[i]], "-- ")
    }
    cat("\n")
    ################################################
    
    
    ################################################
    #For each boat find the sp from the BS or BSs
    ################################################
    
    
    #We need an array or a vector to store the rescuePaths for each Boat!!!
    #Later we will check the intersections and stuff
    
    rescuePathList <- list()
    for(i in (1:nBT)){
      
      BTlabel <- paste0("B",i)
      BTvtxId <- nBSandDepot + i
      #First check if the boat and BS are in the same CC
      BTccId <- cl$membership[BTvtxId]
      
      if (BTccId == BSccId){
        #DEBUG
        cat("Boat",i,"is in the same CC with BS.\n")
        #Now check if the all edges are red
        #If there is no gray edge than the boat can not be saved
        #THIS IS WITHOUT CONSIDERING THE BOAT CHAIN!!!
        
        
        
        edgeList <- incident(gTemp, BTvtxId)
        nEdge <- length(edgeList)
        neighborList <- neighbors(gTemp, BTvtxId)
        nNeighbor <- length(neighborList)
        
        minGrayEdgeWeight <- Inf
        minGrayEdgeSrcVtxId <- -1 #The dest vertex is the Boat!!!
        
        foundGray <-0 
        for (k in (1:nEdge)) {
          
          #DEBUG
          cat("Boat",i,"has total",nEdge,"incident edges. Incident edge" ,k,
              "from vtx",ends(gTemp, edgeList[k])[1] ,"with weight",edgeList[k]$weight,
              "color", edgeList[k]$color,
              "rescuedist",rescueDist,"\n")
          if (edgeList[k]$weight <= rescueDist) {
            foundGray <- 1
            #break
            if (edgeList[k]$weight < minGrayEdgeWeight){
              minGrayEdgeWeight <- edgeList[k]$weight
              minGrayEdgeSrcVtxId <- ends(gTemp, edgeList[k])[1]
            }
            
          }
          
        }
        
        if (foundGray == 1) {
          #Weighted and unweighted shortest paths from BS=1 to the boat
          wSPs <- get.shortest.paths(gTemp, 1, BTvtxId)$vpath
          uwSPs <- get.shortest.paths(gTemp, 1, BTvtxId, weights=NA)$vpath 
          #get the index of the first path that has min number of hops
          minHopPathIdx <- which.min(sapply(wSPs,length))
          wSP <- unlist(wSPs[[minHopPathIdx]])
          
          #The last edge of the wSP can be a red edge (> rescueDist)!!!
          #This is possible if there is a gray path and the total
          #of the gray+red path is less than or equal(?) to the drone range
          #Otherwise the gray path should be selected!!!
          
          #Check if the sp uses red edge
          lastVTX <- wSP[length(wSP)-1]
          
          #Color of the last edge. BUT CAN BE CHANGED BEFORE THE CHECK!!!!
          #E(gTemp, path=wSP)[length(wSP)-1]$color
          #Weight of the last edge. STABLE ALWAYS!!!
          #E(gTemp, path=wSP)[length(wSP)-1]$weight
          
          
          if (E(gTemp, path=wSP)[length(wSP)-1]$color == "red"){
            #We are using red edge for the sp
            #Now we need to check if there is a gray edge to return to the depot network
            #DEBUG
            cat("wSP uses red edge from vtx", lastVTX, "to boat", i,"\n")
            cat("Searching for gray return edge if total of gray+red <= droneRange.\n")
            cat("Min gray edge has weight:", minGrayEdgeWeight, "and the src vtx is", minGrayEdgeSrcVtxId,".\n")
            totalPathLentoReturn <- minGrayEdgeWeight + E(gTemp, path=wSP)[length(wSP)-1]$weight
            if (totalPathLentoReturn <= input$drone_range) {
              cat("There is a return path via gray vtx",minGrayEdgeSrcVtxId,"total weight=", 
                  totalPathLentoReturn, "drone range", input$drone_range,"\n")
              E(gTemp, c(BTvtxId,minGrayEdgeSrcVtxId))$color <- rescuePathCol
              E(gTemp, c(BTvtxId,minGrayEdgeSrcVtxId))$width <- 1
            }else{
              cat("No return path via gray vtx",minGrayEdgeSrcVtxId,"total weight=", totalPathLentoReturn, "drone range", input$drone_range,"\n")
              #Here generate graph "f" by removing the red edge from the graph "gTemp"
              #or you can set the weight to Inf on f
              #Try the sp again and test stuff again
              #Till you find a better path
              #If this was the only red
              
              #Remove the red edge used in sp from the gTemp
              vno <- as.numeric(lastVTX)
              gTemp <- delete.edges(gTemp, E(gTemp, c(vno, BTvtxId)))
              #DEBUG
              cat("Red edge",vno,"to",BTvtxId,"( Boat",i,") is removed from the graph.\n")
              #ftemp <- f
              #Find new sp on f
              #Check the same conditions
              #Continue till you find a "returnable" sp!!!
              #Remember we have at least single gray edge
              FoundgoodSP <- 0
              
              while(FoundgoodSP == 0){
                
                #Weighted and unweighted shortest paths from BS=1 to the boat
                wSPs <- get.shortest.paths(gTemp, 1, BTvtxId)$vpath
                uwSPs <- get.shortest.paths(gTemp, 1, BTvtxId, weights=NA)$vpath 
                #get the index of the first path that has min number of hops
                minHopPathIdx <- which.min(sapply(wSPs,length))
                wSP <- unlist(wSPs[[minHopPathIdx]])
                
                #The last edge of the wSP can be a red edge (> rescueDist)!!!
                #This is possible if there is a gray path and the total
                #of the gray+red path is less than the drone range
                #Otherwise the gray path should be selected!!!
                
                #Check if the sp uses red edge
                lastVTX <- wSP[length(wSP)-1]
                
                
                if (E(gTemp, path=wSP)[length(wSP)-1]$color == "red"){
                  #We are using red edge for the sp
                  #Now we need to check if there is a gray edge to return to the depot network
                  #DEBUG
                  cat("wSP uses red edge from vtx", lastVTX, "to boat", i,"\n")
                  cat("Searching for gray return edge if total of gray+red <= droneRange.\n")
                  cat("Min gray edge has weight:", minGrayEdgeWeight, "and the src vtx is", minGrayEdgeSrcVtxId,".\n")
                  totalPathLentoReturn <- minGrayEdgeWeight + E(gTemp, path=wSP)[length(wSP)-1]$weight
                  if (totalPathLentoReturn <= input$drone_range) {
                    cat("***There is a return path via gray vtx",minGrayEdgeSrcVtxId,"total weight=", 
                        totalPathLentoReturn, "drone range", input$drone_range,"\n")
                    E(gTemp, c(BTvtxId,minGrayEdgeSrcVtxId))$color <- rescuePathCol
                    E(gTemp, c(BTvtxId,minGrayEdgeSrcVtxId))$width <- 2
                    FoundgoodSP <- 1
                  }else{
                    cat("No return path via gray vtx",minGrayEdgeSrcVtxId,"total weight=", totalPathLentoReturn, "drone range", input$drone_range,"\n")
                    FoundgoodSP <- 0
                    #REmove the red edge used in sp from the gTemp
                    vno <- as.numeric(lastVTX)
                    gTemp <- delete.edges(gTemp, E(gTemp, c(vno, BTvtxId)))
                    #f <- ftemp
                    
                  }#else
                  
                }else{
                  cat ("*** From rescueRedGray1by1 *** wSP uses gray edge from vtx", lastVTX, "to boat", i,"\n")  
                  FoundgoodSP <- 1
                }
              }#while
            }
          }else{
            cat ("*** From rescueRedGray1by1 *** wSP uses gray edge from vtx", lastVTX, "to boat", i,"\n")  
            
          }
          
          
          
          #Unweighted paths they all have the same hops!!!!
          #So just take the first one!!!
          uwSP <- unlist(uwSPs[[1]])
          #If the boat is directly reachable from BS gives error!
          #"Error in E: At type_indexededgelist.c:1173 : Cannot get edge id, no such edge, Invalid value"
          
          #E(gTemp)$color <<- "green"
          E(gTemp, path=wSP)$color <- rescuePathCol
          E(gTemp, path=wSP)$width <- 2
          
          #DEBUG
          #E(gTemp, path=uwSP)$color <<- yellowEdgeCol
          #E(gTemp, path=uwSP)$width <<- 3
          
          
          #Finally top plot the changes on the local copy of the graph assign it to reactive myGraph
          myGraph$value <- gTemp
          
          #plot(gTemp, vertex.size=15, edge.label = E(gTemp)$weight, layout=layout_with_graphopt(gTemp))
          #plot(gTemp, vertex.size=15, edge.label = E(gTemp)$weight, layout=myCoords)
          
          spHops <- length(wSP) - 1
          spDist <- sum(E(gTemp, path=unlist(wSP))$weight)
          
          rescuePathList[[i]] <- wSP
          
          if (spHops == 1) {
            my_text <- paste(my_text,"Boat",i,"Shortest rescue path directly from BS! Hops:",spHops,"Distance:",round(spDist,nPrecDigit),"meters.\n")
            totRescueDist <- totRescueDist + spDist
          } else {
            minPathStr <- paste(wSP$label,collapse = " -> ")
            my_text <- paste(my_text,"Boat",i,"Shortest rescue path from BS",spHops,"hops with distance (drone range) =",round(spDist/input$drone_range,nPrecDigit),":", minPathStr,"\n")
            totRescueDist <- totRescueDist + spDist
          }
          
          cat(my_text)
          output$msg1 <- renderText({my_text})
          
        }else{
          #DEBUG
          my_text <- paste(my_text,"Boat",i,"has no gray rescue edge.\n")
          cat(my_text)
          output$msg1 <- renderText({my_text})
          rescuePathList[[i]] <- -1
        }
        
        
      }else{
        #DEBUG
        my_text <- paste(my_text,"Boat",i,"can not be saved. It is not in the same CC with BS.\n")
        cat(my_text)
        output$msg1 <- renderText({my_text})
        rescuePathList[[i]] <- -1
      }
      
    }#for
    
    
    my_text <- paste(my_text,
                     "----------------------------------------\n",
                     "Total rescue path necessary for",nBT,"boats:", round(totRescueDist,nPrecDigit),"m",round(totRescueDist/input$drone_range,nPrecDigit),"drone range\n")
    output$msg1 <- renderText({my_text})
    ################################################
    #Do some rescuePath analysis
    ################################################
    
    if (nBT > 1){
      cat("analyzing for", nBT,"boats rescue paths.\n")
      for (i in (1:(nBT-1))){
        for (j in ((i+1):nBT)){
          cat("Boat",i,"and",j,"analysis:\n")
          #What about crossing edges: Edges with single common vertex?
          if (rescuePathList[[i]][1] != -1  && rescuePathList[[j]][1] != -1 ){
            PathIntersect <- intersection(rescuePathList[[i]], rescuePathList[[j]])
            cat("Intersect:",PathIntersect,"\n")
          }else{
            cat("One boat can not be saved\n")
          }
        }
      }
    }else{
      cat("Just a single boat no need rescue path analysis.\n")
    }
    
    
    ################################################
    
    
    isolate({
      new_zoom <- 9
      if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
    })
    cat("Rescue Lines are drawn.\n")
    toc()
  })
  ########################################################################################################

  
  ########################################################################################################
  #END rescueGray1by1
  ########################################################################################################
  
  
  
  
  
  
  
  
  
  
  ########################################################################################################
  #BEGIN Standard TSP 
  ########################################################################################################
  #OK uses gTemp
  #OK infotext prints the method name
  ########################################################################################################
  ########################################################################################################
  xTSPAlgo <- function(edgeType, tspMethod) {
    
    
    #Assuming that we have the graph save local copy to work on it
    #Both for CW and ACW
    
    if (edgeType == "g"){
      algo <- paste0("xTSPGray-",tspMethod)
      gTempCW <- gGray
      gTempACW <- gGray
    }else if (edgeType == "rg"){
      algo <- paste0("xTSPRedGray-",tspMethod)
      gTempCW <- gRedGray
      gTempACW <- gRedGray
    }
    
    
    cat("****************> Standard TSP:", algo,"\n")
    
    # 
    # algo <- 
    #   switch(input$download,
    #          "euro" = euro,
    #          "mtcars" = mtcars,
    #          "iris" = iris)
    # 
    
    
    
    cat("\n*********************************BEGIN",algo,"*****************************************\n")
    
    tic(algo)
    
    
    cat(algo,": Permutation", nBT ,"boats with",nBSandDepot,"bs+depots.\n")
    
    currentDroneRange <- reac()$drone_range
    
    
    
    
    
    # updateRadioButtons(
    #   session = session, 
    #   inputId = "edgeFilter",
    #   selected = "g"
    # )
    
    # ################################################
    # #CC Analysis of the graph. Good for multiple BSs
    # ################################################
    # #Here check if depots are connected or isolated label them!!!
    # cl <- components(gTemp)
    # #DEBUG
    # cat("There are", cl$no, "CCs in the graph\n");
    # cclist <- lapply(seq_along(cl$csize)[cl$csize > 0], function(x) V(gTemp)$label[cl$membership %in% x])
    # 
    # #Lets find the group BS is in!!!
    # BSccId <- which("BS1" %in% V(gTemp)$label[cl$membership])
    # #May be BSccId <- cl$membership[1]
    # 
    # ################################################
    # #DEBUG
    # # cat("CClist:", unlist(cclist), "\n")
    # cat("CCs are:\n")
    # cat("--------\n")
    # for (i in (1:length(cclist))) {
    #   cat("CC",i,"--",cclist[[i]], "\n")
    # }
    # cat("\n")
    # ################################################
    
    
    
    
    
    
    
    
    #########################################################################################
    #BEGIN: For many boat one BS
    #########################################################################################
    #First you need to update the adjacency mtx
    #For this you need to find chains of boat that a drone can traverse without refueling
    #Think of the algo to do this in an efficient way
    #Memory should not be a problem so find the fastest way
    #Create matrix for boat-to-boat distances
    #How about thinking of the completely connected graph of boats only
    #The goal:
    #Find the shortest path that can contain the most number of boats
    #The path length should be limited to the drone range
    #At the end of the path and at the beginning you should have depots(fueling stations)
    #So basically you find a path between two depots, "drone-range" apart
    #and in between you squeeze as many boat as you can
    #Obviously you need to consider starting with two boats that they have distance
    #less than the drone range and you can connect them to depots
    #
    
    
    #DEBUG cat("NBoat-1BS\n")
    #Clear message area 
    # my_text <-""
    # output$msg1<- renderText({  })
    
    
    boatVids <- c((nDT + nBS + 1) : (nDT + nBS + nBT) ) # vector for vertex ids
    cat("Boat vertex ids:",boatVids,"\n")
    
    infoTxt <- paste0("---------------------------------------------\n",
                      algo,":\n",
                      "---------------------------------------------\n",
                      "BS: ",1," Depots: ", nDT, " Boats: ", nBT, "\n",
                      "---------------------------------------------\n")
    
    
    cat("\n******************************************BEGIN: xTSP****************************************************\n")
    #########################################################################################################
    #BEGIN xTSP stuff for finding the good permutation here
    #########################################################################################################
    
    
    
    #Init variables
    ################################################
    
    #Add BS as the first vertex!!!
    theVertexMTX <- rbind(BSMtx, BTMtx)
    theTSPcoordDF <- as.data.frame(theVertexMTX)
    #insert vertex ids
    theTSPcoordDF[,3] <- c(1,boatVids)
    colnames(theTSPcoordDF) <- c("x", "y", "vno")
    
    theTSPcoordMTX <- as.matrix(theTSPcoordDF)
    
    
    #theDistMTX <- as.matrix(dist(theTSPcoordMTX[,1:2], diag = TRUE, upper = TRUE))
    
    theDistMTX <- distm(theVertexMTX, theVertexMTX, distGeo)
    diag(theDistMTX) <- Inf #So that min dist will not be the self loop
    
    theTSPObject <- TSP::TSP(theDistMTX, labels = theTSPcoordDF$vno)
    nnOptimTour <- TSP::solve_TSP(theTSPObject, method = tspMethod)
    
    cat("*** xTSP: The tour is:", labels( nnOptimTour), "length:", tour_length(nnOptimTour))
    #Lets find the vertex ids on the tour
    #nnRingMergeNearestPtV2TourLength <-  nrow(mergedRingPtsMTX)
    tourVTXIds <- labels( nnOptimTour)
    
    
    tourLen <- length(tourVTXIds)
    theBSpos <- which(tourVTXIds == 1)
    
    #Just throw away the BS label which is 1 and consider boat labels in the tour!!!
    if (theBSpos == 1)     {
      tourCW <- tourVTXIds[2:tourLen]
      tourACW <- tourVTXIds[tourLen:2]
    } else  if (theBSpos == tourLen) {
      tourCW <- tourVTXIds[1:(tourLen-1)]
      tourACW <- tourVTXIds[(tourLen-1):1]
    } else {
      tourCW <- c(tourVTXIds[(theBSpos+1):tourLen], tourVTXIds[1:(theBSpos-1)])
      tourACW <- c(tourVTXIds[(theBSpos-1):1], tourVTXIds[tourLen:(theBSpos+1)])
    } 
    
    
    
    
    #########################################################################################################
    #END xTSP stuff for finding the good permutation here
    #########################################################################################################
    
    
    cat("\n*******************************************END: xTSP*****************************************************\n")
    
    
    
    ###################################################### BEGIN CLOCKWISE ##########################################################
    cat("CW - Boat vertex ids:",boatVids,"\n")
    boatPermCW <- (as.numeric(tourCW) -  (nDT + nBS))
    cat("CW - Boat perm:",boatPermCW,"\n")
    
    theResultList <- my_get_metrics_permDEBUG(gTempCW, boatPermCW, currentDroneRange)
    # theResult[[1]] <- totPathCost 
    # theResult[[2]] <- totPathLen
    # theResult[[3]] <- AWD 
    # theResult[[4]] <- nCharging
    # theResult[[5]] <- path2print
    
    #DEBUG
    cat("CW my_get_metrics_permDEBUG -- Drone Range is:", currentDroneRange,"m.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm cost is:",theResultList[[1]],"m.",round(theResultList[[1]]/currentDroneRange,nPrecDigit),"du.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm hops:",theResultList[[2]],"\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm AWD:",theResultList[[3]],"m.",round(theResultList[[3]]/currentDroneRange,nPrecDigit), "du.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm chargings:",theResultList[[4]],"\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm opt path:",theResultList[[5]],"\n")
    
    
    
    shortestPathCostCW <- theResultList[[1]]
    shortestPathLenCW <- theResultList[[2]]
    AWDCW <- theResultList[[3]]
    nChargingCW <- theResultList[[4]]
    shortestPathCW <- theResultList[[5]]
    shortestRescuePathListCW <- theResultList[[6]]
    shortestRescuePathListLenCW <- theResultList[[7]]
    
    #get the vector showing the order of rescue (permutation)
    rescueOrdCW <- boatPermCW + nBSandDepot
    
    printOrdCW <- sprintf("%s%d","B",(rescueOrdCW - (nDT + nBS)))
    
    
    
    infoTxt <- paste0(infoTxt,"ClockWise order: ",paste0(printOrdCW,collapse = "->"),"\n",
                      "Drone range: ", currentDroneRange,"m\n",
                      "The cost: ",round(shortestPathCostCW,nPrecDigit),"m, hops: ", shortestPathLenCW,"\n",
                      "AWD: ",round(AWDCW,nPrecDigit),"m\n",
                      "Number of full chargings: ",nChargingCW,"\n",
                      "The Path: ",shortestPathCW,"\n",
                      "---------------------------------------------\n")
    
    
    
    
    
    cat("\n*********************************END CLOCKWISE*****************************************\n")
    
    ###################################################### END CLOCKWISE ############################################################
    
    
    
    
    
    
    
    
    ###################################################### BEGIN ANTI-CLOCKWISE #####################################################
    cat("ACW - Boat vertex ids:",boatVids,"\n")
    boatPermACW <- (as.numeric(tourACW) -  (nDT + nBS))
    cat("ACW - Boat perm:",boatPermACW,"\n")
    theResultList <- my_get_metrics_permDEBUG(gTempACW, boatPermACW, currentDroneRange)
    # theResult[[1]] <- totPathCost 
    # theResult[[2]] <- totPathLen
    # theResult[[3]] <- AWD 
    # theResult[[4]] <- nCharging
    # theResult[[5]] <- path2print
    
    #DEBUG
    cat("ACW my_get_metrics_permDEBUG -- Drone Range is:", currentDroneRange,"m.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm cost is:",theResultList[[1]],"m.",round(theResultList[[1]]/currentDroneRange,nPrecDigit),"du.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm hops:",theResultList[[2]],"\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm AWD:",theResultList[[3]],"m.",round(theResultList[[3]]/currentDroneRange,nPrecDigit), "du.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm chargings:",theResultList[[4]],"\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm opt path:",theResultList[[5]],"\n")
    
    
    shortestPathCostACW <- theResultList[[1]]
    shortestPathLenACW <- theResultList[[2]]
    AWDACW <- theResultList[[3]]
    nChargingACW <- theResultList[[4]]
    shortestPathACW <- theResultList[[5]]
    shortestRescuePathListACW <- theResultList[[6]]
    shortestRescuePathListLenACW <- theResultList[[7]]
    
    #get the vector showing the order of rescue (permutation)
    rescueOrdACW <- boatPermACW + nBSandDepot
    
    printOrdACW <- sprintf("%s%d","B",(rescueOrdACW - (nDT + nBS)))
    
    infoTxt <- paste0(infoTxt,"AntiClockWise order: ",paste0(printOrdACW,collapse = "->"),"\n",
                      "Drone range: ", currentDroneRange,"m\n",
                      "The cost: ",round(shortestPathCostACW,nPrecDigit),"m, hops: ", shortestPathLenACW,"\n",
                      "AWD: ",round(AWDACW,nPrecDigit),"m\n",
                      "Number of full chargings: ",nChargingACW,"\n",
                      "The Path: ",shortestPathACW,"\n",
                      "---------------------------------------------\n")
    
    
    
    
    cat("\n*********************************END ANTI-CLOCKWISE*****************************************\n")
    
    ###################################################### END ANTI-CLOCKWISE #####################################################
    
    
    
    elapsed <- toc()
    
    elapsedSec <- elapsed$toc - elapsed$tic
    cat(algo, elapsedSec, "seconds\n")
    
    
    ###################################################### STAT COMBINED ###########################################################
    
    #Here compare CW and ACW direction data and find the shortest path
    if (round(shortestPathCostACW) < round(shortestPathCostCW)) {
      #Anti ClockWise is better 
      cat(algo,": ACW has shorter dist:",round(shortestPathCostACW,nPrecDigit),"m. Selecting ACW\n")
      infoTxt <- paste0(infoTxt,algo,": ACW has shorter dist:",round(shortestPathCostACW,nPrecDigit),"m. Selecting ACW\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat(algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat(algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
      }else{
        #They are equal
        cat(algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
      }
      
      
      #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
      
      boatOrder <- paste0(printOrdACW,collapse = "-")
      theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostACW, 3), round(AWDACW, 3), 
                                nChargingACW, nBSandDepot, round(elapsedSec, 3),
                                boatPosFileName, boatOrder, shortestPathACW, sep=", ")
      
      # #Here color the path
      # for (cnt in (1:shortestRescuePathListLenACW)) {
      #   E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$color <- rescuePathCol
      #   E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$width <- 2
      # }
      # myGraph$value <- gTempACW
    }else if(round(shortestPathCostACW) > round(shortestPathCostCW)) {
      #ClockWise is better
      cat(algo,": CW has shorter dist:",round(shortestPathCostCW,nPrecDigit),"m. Selecting CW\n")
      infoTxt <- paste0(infoTxt,algo,": CW has shorter dist:",round(shortestPathCostCW,nPrecDigit),"m. Selecting CW\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat(algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat(algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
      }else{
        #They are equal
        cat(algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
      }
      
      
      
      #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
      
      boatOrder <- paste0(printOrdCW,collapse = "-")
      theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostCW, 3), round(AWDCW, 3), 
                                nChargingCW, nBSandDepot, round(elapsedSec, 3),
                                boatPosFileName, boatOrder, shortestPathCW, sep=", ")
      
      # #Here color the path
      # for (cnt in (1:shortestRescuePathListLenCW)) {
      #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
      #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
      # }
      # myGraph$value <- gTempCW
    }else{
      #They are equal so choose the one with better AWD
      cat(algo,": Both ACW and CW has same dist:",round(shortestPathCostCW,nPrecDigit),"m\n")
      infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same dist:",round(shortestPathCostCW,nPrecDigit),"m\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat(algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m. Selecting ACW\n")
        infoTxt <- paste0(infoTxt,algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m. Selecting ACW\n")
        
        
        #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
        
        boatOrder <- paste0(printOrdACW,collapse = "-")
        theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostACW, 3), round(AWDACW, 3), 
                                  nChargingACW, nBSandDepot, round(elapsedSec, 3),
                                  boatPosFileName, boatOrder, shortestPathACW, sep=", ")
        
        # #Here color the path
        # for (cnt in (1:shortestRescuePathListLenACW)) {
        #   E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$color <- rescuePathCol
        #   E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$width <- 2
        # }
        # myGraph$value <- gTempACW
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat(algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m. Selecting CW\n")
        infoTxt <- paste0(infoTxt,algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m. Selecting CW\n")
        
        
        #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
        
        boatOrder <- paste0(printOrdCW,collapse = "-")
        theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostCW, 3), round(AWDCW, 3), 
                                  nChargingCW, nBSandDepot, round(elapsedSec, 3),
                                  boatPosFileName, boatOrder, shortestPathCW, sep=", ")
        
        # #Here color the path
        # for (cnt in (1:shortestRescuePathListLenCW)) {
        #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
        #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
        # }
        # myGraph$value <- gTempCW
      }else{
        #They are equal
        cat(algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m choosing CW\n")
        infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m choosing CW\n")
        
        
        #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
        
        boatOrder <- paste0(printOrdCW,collapse = "-")
        theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostCW, 3), round(AWDCW, 3), 
                                  nChargingCW, nBSandDepot, round(elapsedSec, 3),
                                  boatPosFileName, boatOrder, shortestPathCW, sep=", ")
        # #Here color the path
        # for (cnt in (1:shortestRescuePathListLenCW)) {
        #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
        #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
        # }
        # myGraph$value <- gTempCW
      }
      
      
      
      
      # #Here color the path
      # for (cnt in (1:shortestRescuePathListLenCW)) {
      #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
      #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
      # }
      # myGraph$value <- gTempCW
    }
    
    infoTxt <- paste0(infoTxt,"---------------------------------------------\n")
    
    
    
    
    
    #infoTxt <- paste0(infoTxt,"---------------------------------------------\n")
    
    ###################################################### STAT COMBINED ###########################################################
    # output$info<- renderUI(
    #   HTML(
    #     paste(
    #       c("<pre>", capture.output(cat(infoTxt)), "</pre>"),
    #       collapse = "<br>"
    #     )
    #   )
    # )
    
    cat(infoTxt,"\n")
    
    
    #########################################################################################
    #END: For many boat one BS
    #########################################################################################
    
    # isolate({
    #   new_zoom <- 9
    #   if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
    # })
    
    # cat("Rescue Lines are drawn.\n")
    
    
    #Here color the path
    for (cnt in (1:shortestRescuePathListLenCW)) {
      E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
      E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
    }
    myGraph$value <- gTempCW
    
    
    cat(theRowForResults ,"\n",file=resultFile,append=T)
    cat(algo, elapsedSec, "seconds\n")
    
    cat("\n*********************************END",algo,"*****************************************\n")
  }
  ########################################################################################################
  
  #Calling wrappers for button press
  ######################################################################################################## 
  observeEvent(input$xTSPRedGray,{
    cat("\n Calling  xTSPAlgo(rg)\n")
    
    xTSPAlgo("rg",input$tspAlgo)
  })
  
  
  observeEvent(input$xTSPGray,{
    cat("\n Calling  xTSPAlgo(g)\n")
    
    xTSPAlgo("g",input$tspAlgo)
  })
  
  ######################################################################################################## 
  
  
  ########################################################################################################
  #END NN TSP 
  ########################################################################################################
  
  
  
  
  
  
  
  
  
  
  
  
  ########################################################################################################
  #BEGIN CONCAVE TSP
  ########################################################################################################
  #OK uses gTemp
  #OK infotext prints the method name
  ########################################################################################################
  ########################################################################################################
  concaveTSPAlgo <- function(edgeType) {
    
    
    #Assuming that we have the graph save local copy to work on it
    #Both for CW and ACW
    
    if (edgeType == "g"){
      algo <- "concaveTSPGray"
      gTempCW <- gGray
      gTempACW <- gGray
    }else if (edgeType == "rg"){
      algo <- "concaveTSPRedGray"
      gTempCW <- gRedGray
      gTempACW <- gRedGray
    }
    
    cat("\n*********************************BEGIN",algo,"*****************************************\n")
    
    tic(algo)
    
    
    cat(algo,": Permutation", nBT ,"boats with",nBSandDepot,"bs+depots.\n")
    
    currentDroneRange <- reac()$drone_range
    
    
    
    
    
    # updateRadioButtons(
    #   session = session, 
    #   inputId = "edgeFilter",
    #   selected = "g"
    # )
    
    # ################################################
    # #CC Analysis of the graph. Good for multiple BSs
    # ################################################
    # #Here check if depots are connected or isolated label them!!!
    # cl <- components(gTemp)
    # #DEBUG
    # cat("There are", cl$no, "CCs in the graph\n");
    # cclist <- lapply(seq_along(cl$csize)[cl$csize > 0], function(x) V(gTemp)$label[cl$membership %in% x])
    # 
    # #Lets find the group BS is in!!!
    # BSccId <- which("BS1" %in% V(gTemp)$label[cl$membership])
    # #May be BSccId <- cl$membership[1]
    # 
    # ################################################
    # #DEBUG
    # # cat("CClist:", unlist(cclist), "\n")
    # cat("CCs are:\n")
    # cat("--------\n")
    # for (i in (1:length(cclist))) {
    #   cat("CC",i,"--",cclist[[i]], "\n")
    # }
    # cat("\n")
    # ################################################
    
    
    
    
    
    
    
    
    #########################################################################################
    #BEGIN: For many boat one BS
    #########################################################################################
    #First you need to update the adjacency mtx
    #For this you need to find chains of boat that a drone can traverse without refueling
    #Think of the algo to do this in an efficient way
    #Memory should not be a problem so find the fastest way
    #Create matrix for boat-to-boat distances
    #How about thinking of the completely connected graph of boats only
    #The goal:
    #Find the shortest path that can contain the most number of boats
    #The path length should be limited to the drone range
    #At the end of the path and at the beginning you should have depots(fueling stations)
    #So basically you find a path between two depots, "drone-range" apart
    #and in between you squeeze as many boat as you can
    #Obviously you need to consider starting with two boats that they have distance
    #less than the drone range and you can connect them to depots
    #
    
    
    #DEBUG cat("NBoat-1BS\n")
    #Clear message area 
    # my_text <-""
    # output$msg1<- renderText({  })
    
    
    boatVids <- c((nDT + nBS + 1) : (nDT + nBS + nBT) ) # vector for vertex ids
    cat("Boat vertex ids:",boatVids,"\n")
    
    infoTxt <- paste0("---------------------------------------------\n",
                      algo,":\n",
                      "---------------------------------------------\n",
                      "BS: ",1," Depots: ", nDT, " Boats: ", nBT, "\n",
                      "---------------------------------------------\n")
    
    
    cat("\n******************************************BEGIN: concaveTSP****************************************************\n")
    #########################################################################################################
    #BEGIN concaveTSP stuff for finding the good permutation here
    #########################################################################################################
    
    
    
    #Init variables
    ################################################
    
    #FOR BOATS WATCH OUT: VERTEX NOS DO NOT START FROM 1
    #MAKE BS AS VNO 1 AND ADD OTHER BOATS FROM 2!!!!!!!
    
    #Add BS as the first vertex!!!
    theVertexMTX <- rbind(BSMtx, BTMtx)
    theTSPcoordDF <- as.data.frame(theVertexMTX)
    #insert vertex ids
    #ERRROR: theTSPcoordDF[,3] <- c(1,boatVids)
    
    #BS is 1,
    #Boats start from 2 to nBT + 1
    theTSPcoordDF[,3] <- 1:(nBT+1)
    colnames(theTSPcoordDF) <- c("x", "y", "vno")
    
    theTSPcoordMTX <- as.matrix(theTSPcoordDF)
    
    
    
    #HERE: Make sure the boat ids vnos for theDistMTX are correctly set
    
    
    #theDistMTX <- as.matrix(dist(theTSPcoordMTX[,1:2], diag = TRUE, upper = TRUE))
    
    #The following excludes the BS was that error?
    #theDistMTX <- distm(BTMtx, BTMtx, distGeo)
    
    theDistMTX <- distm( theVertexMTX,  theVertexMTX, distGeo)
    
    diag(theDistMTX) <- Inf #So that min dist will not be the self loop
    
    mergedRingPtsMTX <- NULL
    ptsToGoMTX <- theTSPcoordMTX
    #in polygons we will not have vertex ids but in PtsDF!!!
    ringMTXList <- list()
    
    
    
    
    
    #Start processing
    ################################################
    #########################################################################################################
    #The loop for deleting chull repeatedly till all the points are covered
    #########################################################################################################
    
    
    iter <- 1
    while (nrow(ptsToGoMTX) > 2) {
      
      #Attention concaveman returns closed-cyclical mtx
      #All depends on that cr value
      #As it goes less than 1 more points and less number of rings 
      
      #chullMTX is cyclical!!!
      chullMTX <- concaveman(ptsToGoMTX, concavity = cr)
      nChullPts <- nrow(chullMTX) - 1
      if (nChullPts <= 2){
        #It is possible there are hundreds of points linear
        #In this case the longest line with the two end pointas will be the concavehull
        #So we will have 2 points
        #Time to break
        cat("Iter",iter,"breaking the loop!\n")
        break
      }
      
      
      
      #RingPtsMTXcontains all the points on ring no repetition of the first vtx at the end
      RingPtsMTX <- chullMTX[1:(nrow(chullMTX)-1),]
      ringMTXList[[iter]] <- chullMTX
      #Precision figures can be problem in join so lets normalize them
      RingPtsMTX <- round(RingPtsMTX, coordDigitPrec)
      ptsToGoMTX <- round(ptsToGoMTX, coordDigitPrec)
      #Now subtract the ring  points from the TSP pts and find new ring to do the same
      
      #Both  ptsToGoMTX and RingPtsMTXare not cyclical!!!
      
      #ptsToGoMTX <- anti_join(ptsToGoMTX, RingPtsMTX, by = c("x" = "x", "y" = "y", "vno" = "vno"))
      ptsToGoDF <- as.data.frame(ptsToGoMTX)
      colnames(ptsToGoDF) <-  c("x", "y", "vno")
      
      RingPtsDF <- as.data.frame(RingPtsMTX)
      colnames(RingPtsDF) <-  c("x", "y", "vno")
      
      ptsToGoMTX <- as.matrix(anti_join(ptsToGoDF, RingPtsDF, by = c("x" = "x", "y" = "y", "vno" = "vno")))
      
      iter <- iter + 1
    }
    
    ####################################################################
    #DO NOT FORGET TO ADD THEM!!!
    ptsNotInAnyRingMTX <- ptsToGoMTX
    ####################################################################
    lenRemained <- nrow(ptsToGoMTX)
    
    if (lenRemained > 0) { 
      cat(lenRemained,"pts remained that are not in any chull.\n")
    }else{
      cat("All pts are in chulls.\n")
    }
    
    nRings <- length(ringMTXList)
    
    cat(nRings, "rings generated.\n")
    
    
    
    #########################################################################################################
    #Now unify the rings to single one: One ring to rule them all!!!
    #########################################################################################################
    # Watch the boundary cases!!!
    # When one of the pair is first or the last vertex you need to erase the closing duplicate vertex stuff 
    # and update the rings (polygons or chulls)!!!
    #Loop over rings
    # The first ring is the outmost one
    # All others are inside of it
    # In this loop slowly merge them to the outermost ring
    #ATTENTION:
    #The last and the first pts are same for polygons
    #For all other structures throw the last pts away
    
    
    #Put the vertices of the outmost ring to the "merged Ring" (the ruler ring!!!)
    mergedRingMTX <- ringMTXList[[1]]
    nr <- nrow(mergedRingMTX)
    mergedRingPtsMTX <- mergedRingMTX[1:(nr-1),]
    #Pts of the mergedRingPtsMTX so far
    nPts <- nrow(mergedRingPtsMTX)
    #Save the vertex ids and update this vector as more pts are added
    mergedRingPtsVnoVec <- c(mergedRingPtsMTX[, 3])  
    
    
    
    #Start from the second ring, if any ring left after concave hull, and add them into the merged one
    
    if (nRings >= 2){
      for (r in 2:nRings) {
        
        #DEBUG
        #r <- 2
        
        
        #Put ring points into a buffer as they  will be updated later and the id numbers will change
        ringToBeMergedPtsMTX <- ringMTXList[[r]]
        #Take out the last points as it is the same with the first one
        ringToBeMergedPtsMTX <- ringToBeMergedPtsMTX[1:(nrow(ringToBeMergedPtsMTX)-1), ]
        nPtsToBeMerged <- nrow(ringToBeMergedPtsMTX)
        
        
        #Loop over points on the next -- inner ring
        #Some ideas to think:
        #The problem is where to start and should we connect every point individually or in chunks of chain?
        #Starting from the closest points?
        #If the point is furthest away from a threshold skip it?
        for (p in (1:nPtsToBeMerged)) {
          
          #DEBUG
          #p <- 5
          
          #DEBUG
          #p <- 1
          
          
          ################################################################################## 
          #DEBUG plot
          # mergedPts <- matrix(ncol=3)
          ################################################################################## 
          
          
          
          ##################################################################################  
          #Merging criteria: Closest point from the MergedRing
          ##################################################################################
          #Try to find the nearest pt from the merged ring
          #ATTENTION: The merged ring changes after each point addition
          #Number of distinct points in the ring,
          #excluding the last one since it is same with the first one (polygons and rings are closed)
          n <- nrow(mergedRingPtsMTX)
          
          PtToBeMerged <- ringMTXList[[r]][p,]
          PtToBeMergedVno <- ringMTXList[[r]][p,3]
          
          #The min pt should also be on the merged ring
          minPtidx <- which.min(theDistMTX[PtToBeMergedVno, mergedRingPtsVnoVec])
          minPtVno <- mergedRingPtsVnoVec[minPtidx]
          
          nearestPtsIdx <- minPtidx
          nearestPtsVno <- minPtVno
          
          
          
          ##################################################################################
          #Now we find the nearest pt on the mergedRing to connect the pt.
          
          
          if (nearestPtsIdx == 1) {
            #The nearest pt is the first pt in mergedRing
            #The previous point is the penultimate point of the merged ring
            #Attention the last one and the first one is same!!!
            preNearestIdx <- n
            preNearestVno <- mergedRingPtsMTX[n, 3]
            postNearestIdx <- 2 
            postNearestVno <- mergedRingPtsMTX[2, 3]
            
            #Get the distances to see which way is the cheapest to connect!!!
            #Before the nearest pt or after the nearest pt!!!
            #Distance between pre and the nearest
            preDist <- theDistMTX[nearestPtsVno,  preNearestVno]
            #Distance between post and the nearest
            postDist <- theDistMTX[nearestPtsVno,  postNearestVno]
            #Distance between the point to be merged and the nearest
            nearest2ptDist <- theDistMTX[PtToBeMergedVno, nearestPtsVno]
            #Distance between the pre of the nearest and pt to be merged
            pre2ptDist <- theDistMTX[PtToBeMergedVno, preNearestVno]
            #Distance between the post of the nearest and pt to be merged
            post2ptDist <- theDistMTX[PtToBeMergedVno, postNearestVno]
            
            if ((preDist + post2ptDist) < (postDist + pre2ptDist)){
              #after the nearest
              #DEBUG
              #cat("1: nearest first after nearest\n")
              mergedRingPtsMTX <- rbind(mergedRingPtsMTX[1:nearestPtsIdx,], PtToBeMerged, mergedRingPtsMTX[(nearestPtsIdx + 1):n,] )
              #Update vnovector too
              mergedRingPtsVnoVec <- c(mergedRingPtsVnoVec[1:nearestPtsIdx], PtToBeMergedVno, mergedRingPtsVnoVec[(nearestPtsIdx + 1):n])
            }else{
              #before the nearest
              #DEBUG
              #cat("2: nearest first before nearest\n")
              mergedRingPtsMTX <- rbind(PtToBeMerged, mergedRingPtsMTX[1:n,])
              #Update vnovector too
              mergedRingPtsVnoVec <- c(PtToBeMergedVno, mergedRingPtsVnoVec)
            }
            
            
            
          }else if (nearestPtsIdx == n) {#The nearest pt is the last pt in mergedRing or
            
            preNearestIdx <- nearestPtsIdx - 1 
            preNearestVno <- mergedRingPtsMTX[nearestPtsIdx - 1, 3]
            
            #The next point of the last one is the first point!!!
            
            postNearestIdx <- 1
            postNearestVno <- mergedRingPtsMTX[1, 3]
            
            #Get the distances to see which way is the cheapest to connect!!!
            #Before the nearest pt or after the nearest pt!!!
            #Distance between pre and the nearest
            preDist <- theDistMTX[nearestPtsVno,  preNearestVno]
            #Distance between post and the nearest
            postDist <- theDistMTX[nearestPtsVno,  postNearestVno]
            #Distance between the point to be merged and the nearest
            nearest2ptDist <- theDistMTX[PtToBeMergedVno, nearestPtsVno]
            #Distance between the pre of the nearest and pt to be merged
            pre2ptDist <- theDistMTX[PtToBeMergedVno, preNearestVno]
            #Distance between the post of the nearest and pt to be merged
            post2ptDist <- theDistMTX[PtToBeMergedVno, postNearestVno]
            if ((preDist + post2ptDist) < (postDist + pre2ptDist)){
              #after the nearest
              #DEBUG
              #cat("3: nearest last after nearest\n")
              mergedRingPtsMTX <- rbind(mergedRingPtsMTX[1:nearestPtsIdx,], PtToBeMerged)
              #Update vnovector too
              
              #PtToBeMerged will be the last element
              mergedRingPtsVnoVec <- c(mergedRingPtsVnoVec, PtToBeMergedVno)
              
            }else{
              #DEBUG
              #cat("4: nearest last before nearest\n")
              #before the nearest
              #update the last one since it will be the first point in the ring/polygon
              mergedRingPtsMTX <- rbind(mergedRingPtsMTX[1:(nearestPtsIdx-1),], PtToBeMerged, mergedRingPtsMTX[nearestPtsIdx:n,])
              #Update vnovector too too
              mergedRingPtsVnoVec <- c(mergedRingPtsVnoVec[1:(nearestPtsIdx-1)], PtToBeMergedVno, mergedRingPtsVnoVec[nearestPtsIdx:n])
            }
            
            
            
            
            
          } else{ #The nearest (from the mergedRing) pt is in between
            #The nearest pt is the last pt in mergedRing or
            #The nearest (from the mergedRing) pt is in between
            preNearestIdx <- nearestPtsIdx - 1 
            preNearestVno <- mergedRingPtsMTX[nearestPtsIdx - 1, 3]
            
            
            postNearestIdx <- nearestPtsIdx + 1
            postNearestVno <- mergedRingPtsMTX[nearestPtsIdx + 1, 3]
            
            
            #Get the distances to see which way is the cheapest to connect!!!
            #Before the nearest pt or after the nearest pt!!!
            #Distance between pre and the nearest
            preDist <- theDistMTX[nearestPtsVno,  preNearestVno]
            #Distance between post and the nearest
            postDist <- theDistMTX[nearestPtsVno,  postNearestVno]
            #Distance between the point to be merged and the nearest
            nearest2ptDist <- theDistMTX[PtToBeMergedVno, nearestPtsVno]
            #Distance between the pre of the nearest and pt to be merged
            pre2ptDist <- theDistMTX[PtToBeMergedVno, preNearestVno]
            #Distance between the post of the nearest and pt to be merged
            post2ptDist <- theDistMTX[PtToBeMergedVno, postNearestVno]
            if ((preDist + post2ptDist) < (postDist + pre2ptDist)){
              #after the nearest
              #DEBUG
              #cat("5: nearest between after nearest\n")
              #IF THE NEAREST IS THE LAST THAN (nearestPtsIdx + 1) WILL GIVE ERROR!!!!
              mergedRingPtsMTX <- rbind(mergedRingPtsMTX[1:nearestPtsIdx,], PtToBeMerged, mergedRingPtsMTX[(nearestPtsIdx + 1):n,])
              #Update vnovector too
              
              mergedRingPtsVnoVec <- c(mergedRingPtsVnoVec[1:nearestPtsIdx], PtToBeMergedVno, mergedRingPtsVnoVec[(nearestPtsIdx+1):n])
              
            }else{
              #DEBUG
              #cat("6: nearest between before nearest\n")
              #before the nearest
              #update the last one since it will be the first point in the ring/polygon
              mergedRingPtsMTX <- rbind(mergedRingPtsMTX[1:(nearestPtsIdx-1),], PtToBeMerged, mergedRingPtsMTX[nearestPtsIdx:n,])
              #Update vnovector too too
              mergedRingPtsVnoVec <- c(mergedRingPtsVnoVec[1:(nearestPtsIdx-1)], PtToBeMergedVno, mergedRingPtsVnoVec[nearestPtsIdx:n])
            }
          } #else#else
          
          
          
          
          
          
          
          
          
          
          
          
          
          #############################################################################################################
          #DEBUG plot for merging
          #########################################################################################################
          #################################################################################################### 
          # mergedPts <- rbind(mergedPts, PtToBeMerged)
          # 
          # #For EPS output
          # cairo_ps(filename = (paste0(theTSP,"-merge-ring-", r, "-pt-", p,".eps")),
          #          width = 12,
          #          height = 7,
          #          pointsize = 12,
          #          fallback_resolution = 600)
          # 
          # #Draw the tsp
          # plot(theTSPObject,
          #      xlab = "x-coordinate",
          #      ylab = "y-coordinate",
          #      lty = 3, lwd=0.7, cex=1.5,  col = "black", pch= 19, asp=1,
          #      main = paste0(theTSP," --- Merging ring: ", r, " point: ", p),
          #      xlim=c(xmin, xmax),
          #      ylim=c(ymin, ymax)
          # )
          # 
          # #Specific vertex label positions for pr76
          # text(theTSPcoordMTX[c(1:71,73:75),1], theTSPcoordMTX[c(1:71,73:75),2], labels = c(1:71,73:75), cex = 0.8, col = "blue", pos = 3)
          # text(theTSPcoordMTX[c(72,76),1], theTSPcoordMTX[c(72,76),2], labels = c(72,76), cex = 0.8, col = "blue", pos = 4)
          # 
          # 
          # 
          # 
          # 
          # #mark next ring
          # if (nrow(ringToBeMergedPtsMTX ) > 0){
          #   points(ringToBeMergedPtsMTX[,1], ringToBeMergedPtsMTX[,2], col = "pink", pch = 19, cex=2)
          #   lines(ringToBeMergedPtsMTX[,1], ringToBeMergedPtsMTX[,2], col = "pink", pch = 19)
          #   lines(ringToBeMergedPtsMTX[c(1, nrow(ringToBeMergedPtsMTX)),1], 
          #         ringToBeMergedPtsMTX[c(1, nrow(ringToBeMergedPtsMTX)),2], col = "pink", pch = 19)
          #   text(ringToBeMergedPtsMTX[,1]+1, 
          #        ringToBeMergedPtsMTX[,2]+1,labels=1:nrow(ringToBeMergedPtsMTX), cex=0.75, font=2)
          # }
          # 
          # #Draw the merged ring 
          # colx =  "green" #rainbow(7)[sample(7,1)]
          # points(mergedRingPtsMTX[,1], mergedRingPtsMTX[,2], col = colx, pch = 19, cex=2)
          # lines(mergedRingPtsMTX[,1], mergedRingPtsMTX[,2], col = colx, pch = 19)
          # lines(mergedRingPtsMTX[c(1, nrow(mergedRingPtsMTX)),1], mergedRingPtsMTX[c(1, nrow(mergedRingPtsMTX)),2], col = colx, pch = 19)
          # text(mergedRingPtsMTX[,1]+1, mergedRingPtsMTX[,2]+1, labels=1:nrow(mergedRingPtsMTX), cex=0.75, font=2)
          # 
          # #mark the newly merged next ring vertices if any
          # if (nrow(mergedPts) >= 1){
          #   mergedPts <- matrix(mergedPts[2:nrow(mergedPts),], ncol=3) #remove the first row as it is NAs
          #   points(mergedPts[,1], mergedPts[,2], col = "blue", pch = 19, cex=2)
          # }
          # 
          # #mark the next point?
          # #PtToBeMerged <- st_point(ringToBeMergedPtsMTX[p,])
          # if (p < nPtsToBeMerged) {
          #   points(ringToBeMergedPtsMTX[p+1,1], ringToBeMergedPtsMTX[p+1,2], col="red", pch=19, cex=2)
          # }
          # 
          # dev.off()
          # 
          #############################################################################################################
          
          
          
          
          
          
          
        }#end for loop over ring points
      }#end for loop over rings
      
    }else {
      #Single ring
      cat("Single ring! Checking if there are pts remained.\n")
      
    }
    
    ####################################################################
    #DO NOT FORGET TO ADD THEM!!!
    #ptsNotInAnyRingMTX 
    ####################################################################
    
    lenRemained <- nrow(ptsNotInAnyRingMTX)
    
    if (lenRemained > 0) { 
      cat(lenRemained,"pts remained that are not in any ring.\n")
      ringToBeMergedPtsMTX <- ptsNotInAnyRingMTX
      nPtsToBeMerged <- nrow(ringToBeMergedPtsMTX)
      
      for (k in 1:lenRemained) {
        
        #DEBUG
        #k <- 1
        
        
        ##################################################################################  
        #Merging criteria: Closest point from the MergedRing
        ##################################################################################
        #Try to find the nearest pt from the merged ring
        #ATTENTION: The merged ring changes after each point addition
        #Number of distinct points in the ring,
        #excluding the last one since it is same with the first one (polygons and rings are closed)
        n <- nrow(mergedRingPtsMTX)
        
        
        PtToBeMerged <- ringToBeMergedPtsMTX[k,]
        PtToBeMergedVno <- ringToBeMergedPtsMTX[k,3]
        
        #This is vno we need the boat no for theDistMTX
        
        #The min pt should also be on the merged ring
        minPtidx <- which.min(theDistMTX[PtToBeMergedVno, mergedRingPtsVnoVec])
        minPtVno <- mergedRingPtsVnoVec[minPtidx]
        
        nearestPtsIdx <- minPtidx
        nearestPtsVno <- minPtVno
        
        
        
        ##################################################################################
        #Now we find the nearest pt on the mergedRing to connect the pt.
        
        
        if (nearestPtsIdx == 1) {
          #The nearest pt is the first pt in mergedRing
          #The previous point is the penultimate point of the merged ring
          #Attention the last one and the first one is same!!!
          preNearestIdx <- n
          preNearestVno <- mergedRingPtsMTX[n, 3]
          postNearestIdx <- 2 
          postNearestVno <- mergedRingPtsMTX[2, 3]
          
          #Get the distances to see which way is the cheapest to connect!!!
          #Before the nearest pt or after the nearest pt!!!
          #Distance between pre and the nearest
          preDist <- theDistMTX[nearestPtsVno,  preNearestVno]
          #Distance between post and the nearest
          postDist <- theDistMTX[nearestPtsVno,  postNearestVno]
          #Distance between the point to be merged and the nearest
          nearest2ptDist <- theDistMTX[PtToBeMergedVno, nearestPtsVno]
          #Distance between the pre of the nearest and pt to be merged
          pre2ptDist <- theDistMTX[PtToBeMergedVno, preNearestVno]
          #Distance between the post of the nearest and pt to be merged
          post2ptDist <- theDistMTX[PtToBeMergedVno, postNearestVno]
          
          if ((preDist + post2ptDist) < (postDist + pre2ptDist)){
            #after the nearest
            #DEBUG
            #cat("1: nearest first after nearest\n")
            mergedRingPtsMTX <- rbind(mergedRingPtsMTX[1:nearestPtsIdx,], PtToBeMerged, mergedRingPtsMTX[(nearestPtsIdx + 1):n,] )
            #Update vnovector too
            mergedRingPtsVnoVec <- c(mergedRingPtsVnoVec[1:nearestPtsIdx], PtToBeMergedVno, mergedRingPtsVnoVec[(nearestPtsIdx + 1):n])
          }else{
            #before the nearest
            #DEBUG
            #cat("2: nearest first before nearest\n")
            mergedRingPtsMTX <- rbind(PtToBeMerged, mergedRingPtsMTX[1:n,])
            #Update vnovector too
            mergedRingPtsVnoVec <- c(PtToBeMergedVno, mergedRingPtsVnoVec)
          }
          
          
          
        }else if (nearestPtsIdx == n) {#The nearest pt is the last pt in mergedRing or
          
          preNearestIdx <- nearestPtsIdx - 1 
          preNearestVno <- mergedRingPtsMTX[nearestPtsIdx - 1, 3]
          
          #The next point of the last one is the first point!!!
          
          postNearestIdx <- 1
          postNearestVno <- mergedRingPtsMTX[1, 3]
          
          #Get the distances to see which way is the cheapest to connect!!!
          #Before the nearest pt or after the nearest pt!!!
          #Distance between pre and the nearest
          preDist <- theDistMTX[nearestPtsVno,  preNearestVno]
          #Distance between post and the nearest
          postDist <- theDistMTX[nearestPtsVno,  postNearestVno]
          #Distance between the point to be merged and the nearest
          nearest2ptDist <- theDistMTX[PtToBeMergedVno, nearestPtsVno]
          #Distance between the pre of the nearest and pt to be merged
          pre2ptDist <- theDistMTX[PtToBeMergedVno, preNearestVno]
          #Distance between the post of the nearest and pt to be merged
          post2ptDist <- theDistMTX[PtToBeMergedVno, postNearestVno]
          if ((preDist + post2ptDist) < (postDist + pre2ptDist)){
            #after the nearest
            #DEBUG
            #cat("3: nearest last after nearest\n")
            mergedRingPtsMTX <- rbind(mergedRingPtsMTX[1:nearestPtsIdx,], PtToBeMerged)
            #Update vnovector too
            
            #PtToBeMerged will be the last element
            mergedRingPtsVnoVec <- c(mergedRingPtsVnoVec, PtToBeMergedVno)
            
          }else{
            #DEBUG
            #cat("4: nearest last before nearest\n")
            #before the nearest
            #update the last one since it will be the first point in the ring/polygon
            mergedRingPtsMTX <- rbind(mergedRingPtsMTX[1:(nearestPtsIdx-1),], PtToBeMerged, mergedRingPtsMTX[nearestPtsIdx:n,])
            #Update vnovector too too
            mergedRingPtsVnoVec <- c(mergedRingPtsVnoVec[1:(nearestPtsIdx-1)], PtToBeMergedVno, mergedRingPtsVnoVec[nearestPtsIdx:n])
          }
          
          
          
          
          
        } else{ #The nearest (from the mergedRing) pt is in between
          #The nearest pt is the last pt in mergedRing or
          #The nearest (from the mergedRing) pt is in between
          preNearestIdx <- nearestPtsIdx - 1 
          preNearestVno <- mergedRingPtsMTX[nearestPtsIdx - 1, 3]
          
          
          postNearestIdx <- nearestPtsIdx + 1
          postNearestVno <- mergedRingPtsMTX[nearestPtsIdx + 1, 3]
          
          
          #Get the distances to see which way is the cheapest to connect!!!
          #Before the nearest pt or after the nearest pt!!!
          #Distance between pre and the nearest
          preDist <- theDistMTX[nearestPtsVno,  preNearestVno]
          #Distance between post and the nearest
          postDist <- theDistMTX[nearestPtsVno,  postNearestVno]
          #Distance between the point to be merged and the nearest
          nearest2ptDist <- theDistMTX[PtToBeMergedVno, nearestPtsVno]
          #Distance between the pre of the nearest and pt to be merged
          pre2ptDist <- theDistMTX[PtToBeMergedVno, preNearestVno]
          #Distance between the post of the nearest and pt to be merged
          post2ptDist <- theDistMTX[PtToBeMergedVno, postNearestVno]
          if ((preDist + post2ptDist) < (postDist + pre2ptDist)){
            #after the nearest
            #DEBUG
            #cat("5: nearest between after nearest\n")
            #IF THE NEAREST IS THE LAST THAN (nearestPtsIdx + 1) WILL GIVE ERROR!!!!
            mergedRingPtsMTX <- rbind(mergedRingPtsMTX[1:nearestPtsIdx,], PtToBeMerged, mergedRingPtsMTX[(nearestPtsIdx + 1):n,])
            #Update vnovector too
            
            mergedRingPtsVnoVec <- c(mergedRingPtsVnoVec[1:nearestPtsIdx], PtToBeMergedVno, mergedRingPtsVnoVec[(nearestPtsIdx+1):n])
            
          }else{
            #DEBUG
            #cat("6: nearest between before nearest\n")
            #before the nearest
            #update the last one since it will be the first point in the ring/polygon
            mergedRingPtsMTX <- rbind(mergedRingPtsMTX[1:(nearestPtsIdx-1),], PtToBeMerged, mergedRingPtsMTX[nearestPtsIdx:n,])
            #Update vnovector too too
            mergedRingPtsVnoVec <- c(mergedRingPtsVnoVec[1:(nearestPtsIdx-1)], PtToBeMergedVno, mergedRingPtsVnoVec[nearestPtsIdx:n])
          }
        } #else#else
        
        
        
        
        
        
        
        
      }
      
    }else{
      cat("All pts were in rings.\n")
    }
    
    
    #Lets find the vertex ids on the tour
    concaveRingMergeNearestPtV2TourLength <-  nrow(mergedRingPtsMTX)
    tourVTXIds <- mergedRingPtsMTX[,3]
    
    
    tourLen <- length(tourVTXIds)
    theBSpos <- which(tourVTXIds == 1)
    
    cat("*****DEBUG: After concaveTSP tour the BSPos is found at:", theBSpos, "vids:", tourVTXIds,"\n")
    
    if (theBSpos == 1)     {
      tourCW <- tourVTXIds[2:tourLen]
      tourACW <- tourVTXIds[tourLen:2]
    } else  if (theBSpos == tourLen) {
      tourCW <- tourVTXIds[1:(tourLen-1)]
      tourACW <- tourVTXIds[(tourLen-1):1]
    } else {
      tourCW <- c(tourVTXIds[(theBSpos+1):tourLen], tourVTXIds[1:(theBSpos-1)])
      tourACW <- c(tourVTXIds[(theBSpos-1):1], tourVTXIds[tourLen:(theBSpos+1)])
    } 
    
    
    
    
    #########################################################################################################
    #END concaveTSP stuff for finding the good permutation here
    #########################################################################################################
    
    
    cat("\n*******************************************END: concaveTSP*****************************************************\n")
    
    
    
    ###################################################### BEGIN CLOCKWISE ##########################################################
    cat("CW - Boat vertex ids:",boatVids,"\n")
    boatPermCW <- (tourCW -  nBS)
    cat("CW - Boat perm:",boatPermCW,"\n")
    
    theResultList <- my_get_metrics_permDEBUG(gTempCW, boatPermCW, currentDroneRange)
    # theResult[[1]] <- totPathCost 
    # theResult[[2]] <- totPathLen
    # theResult[[3]] <- AWD 
    # theResult[[4]] <- nCharging
    # theResult[[5]] <- path2print
    
    #DEBUG
    cat("CW my_get_metrics_permDEBUG -- Drone Range is:", currentDroneRange,"m.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm cost is:",theResultList[[1]],"m.",round(theResultList[[1]]/currentDroneRange,nPrecDigit),"du.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm hops:",theResultList[[2]],"\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm AWD:",theResultList[[3]],"m.",round(theResultList[[3]]/currentDroneRange,nPrecDigit), "du.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm chargings:",theResultList[[4]],"\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm opt path:",theResultList[[5]],"\n")
    
    
    
    shortestPathCostCW <- theResultList[[1]]
    shortestPathLenCW <- theResultList[[2]]
    AWDCW <- theResultList[[3]]
    nChargingCW <- theResultList[[4]]
    shortestPathCW <- theResultList[[5]]
    shortestRescuePathListCW <- theResultList[[6]]
    shortestRescuePathListLenCW <- theResultList[[7]]
    
    #get the vector showing the order of rescue (permutation)
    rescueOrdCW <- boatPermCW + nBSandDepot
    
    printOrdCW <- sprintf("%s%d","B",(rescueOrdCW - (nDT + nBS)))
    
    
    
    infoTxt <- paste0(infoTxt,"ClockWise order: ",paste0(printOrdCW,collapse = "->"),"\n",
                      "Drone range: ", currentDroneRange,"m\n",
                      "The cost: ",round(shortestPathCostCW,nPrecDigit),"m, hops: ", shortestPathLenCW,"\n",
                      "AWD: ",round(AWDCW,nPrecDigit),"m\n",
                      "Number of full chargings: ",nChargingCW,"\n",
                      "The Path: ",shortestPathCW,"\n",
                      "---------------------------------------------\n")
    
    
    
    
    
    cat("\n*********************************END CLOCKWISE*****************************************\n")
    
    ###################################################### END CLOCKWISE ############################################################
    
    
    
    
    
    
    
    
    ###################################################### BEGIN ANTI-CLOCKWISE #####################################################
    cat("ACW - Boat vertex ids:",boatVids,"\n")
    boatPermACW <- (tourACW -  nBS)
    cat("ACW - Boat perm:",boatPermACW,"\n")
    theResultList <- my_get_metrics_permDEBUG(gTempACW, boatPermACW, currentDroneRange)
    # theResult[[1]] <- totPathCost 
    # theResult[[2]] <- totPathLen
    # theResult[[3]] <- AWD 
    # theResult[[4]] <- nCharging
    # theResult[[5]] <- path2print
    
    #DEBUG
    cat("ACW my_get_metrics_permDEBUG -- Drone Range is:", currentDroneRange,"m.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm cost is:",theResultList[[1]],"m.",round(theResultList[[1]]/currentDroneRange,nPrecDigit),"du.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm hops:",theResultList[[2]],"\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm AWD:",theResultList[[3]],"m.",round(theResultList[[3]]/currentDroneRange,nPrecDigit), "du.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm chargings:",theResultList[[4]],"\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm opt path:",theResultList[[5]],"\n")
    
    
    shortestPathCostACW <- theResultList[[1]]
    shortestPathLenACW <- theResultList[[2]]
    AWDACW <- theResultList[[3]]
    nChargingACW <- theResultList[[4]]
    shortestPathACW <- theResultList[[5]]
    shortestRescuePathListACW <- theResultList[[6]]
    shortestRescuePathListLenACW <- theResultList[[7]]
    
    #get the vector showing the order of rescue (permutation)
    rescueOrdACW <- boatPermACW + nBSandDepot
    
    printOrdACW <- sprintf("%s%d","B",(rescueOrdACW - (nDT + nBS)))
    
    infoTxt <- paste0(infoTxt,"AntiClockWise order: ",paste0(printOrdACW,collapse = "->"),"\n",
                      "Drone range: ", currentDroneRange,"m\n",
                      "The cost: ",round(shortestPathCostACW,nPrecDigit),"m, hops: ", shortestPathLenACW,"\n",
                      "AWD: ",round(AWDACW,nPrecDigit),"m\n",
                      "Number of full chargings: ",nChargingACW,"\n",
                      "The Path: ",shortestPathACW,"\n",
                      "---------------------------------------------\n")
    
    
    
    
    cat("\n*********************************END ANTI-CLOCKWISE*****************************************\n")
    
    ###################################################### END ANTI-CLOCKWISE #####################################################
    
    
    
    elapsed <- toc()
    
    elapsedSec <- elapsed$toc - elapsed$tic
    cat(algo, elapsedSec, "seconds\n")
    
    
    ###################################################### STAT COMBINED ###########################################################
    
    #Here compare CW and ACW direction data and find the shortest path
    if (round(shortestPathCostACW) < round(shortestPathCostCW)) {
      #Anti ClockWise is better 
      cat(algo,": ACW has shorter dist:",round(shortestPathCostACW,nPrecDigit),"m. Selecting ACW\n")
      infoTxt <- paste0(infoTxt,algo,": ACW has shorter dist:",round(shortestPathCostACW,nPrecDigit),"m. Selecting ACW\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat(algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat(algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
      }else{
        #They are equal
        cat(algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
      }
      
      
      #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
      
      boatOrder <- paste0(printOrdACW,collapse = "-")
      theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostACW, 3), round(AWDACW, 3), 
                                nChargingACW, nBSandDepot, round(elapsedSec, 3),
                                boatPosFileName, boatOrder, shortestPathACW, sep=", ")
      
      #Here color the path
      for (cnt in (1:shortestRescuePathListLenACW)) {
        E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$color <- rescuePathCol
        E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$width <- 2
      }
      myGraph$value <- gTempACW
    }else if(round(shortestPathCostACW) > round(shortestPathCostCW)) {
      #ClockWise is better
      cat(algo,": CW has shorter dist:",round(shortestPathCostCW,nPrecDigit),"m. Selecting CW\n")
      infoTxt <- paste0(infoTxt,algo,": CW has shorter dist:",round(shortestPathCostCW,nPrecDigit),"m. Selecting CW\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat(algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat(algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
      }else{
        #They are equal
        cat(algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
      }
      
      
      
      #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
      
      boatOrder <- paste0(printOrdCW,collapse = "-")
      theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostCW, 3), round(AWDCW, 3), 
                                nChargingCW, nBSandDepot, round(elapsedSec, 3),
                                boatPosFileName, boatOrder, shortestPathCW, sep=", ")

      #Here color the path
      for (cnt in (1:shortestRescuePathListLenCW)) {
        E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
        E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
      }
      myGraph$value <- gTempCW
    }else{
      #They are equal so choose the one with better AWD
      cat(algo,": Both ACW and CW has same dist:",round(shortestPathCostCW,nPrecDigit),"m\n")
      infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same dist:",round(shortestPathCostCW,nPrecDigit),"m\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat(algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m. Selecting ACW\n")
        infoTxt <- paste0(infoTxt,algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m. Selecting ACW\n")
        
        
        #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
        
        boatOrder <- paste0(printOrdACW,collapse = "-")
        theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostACW, 3), round(AWDACW, 3), 
                                  nChargingACW, nBSandDepot, round(elapsedSec, 3),
                                  boatPosFileName, boatOrder, shortestPathACW, sep=", ")
        
        #Here color the path
        for (cnt in (1:shortestRescuePathListLenACW)) {
          E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$color <- rescuePathCol
          E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$width <- 2
        }
        myGraph$value <- gTempACW
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat(algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m. Selecting CW\n")
        infoTxt <- paste0(infoTxt,algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m. Selecting CW\n")
        
        
        #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
        
        boatOrder <- paste0(printOrdCW,collapse = "-")
        theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostCW, 3), round(AWDCW, 3), 
                                  nChargingCW, nBSandDepot, round(elapsedSec, 3),
                                  boatPosFileName, boatOrder, shortestPathCW, sep=", ")
        
        #Here color the path
        for (cnt in (1:shortestRescuePathListLenCW)) {
          E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
          E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
        }
        myGraph$value <- gTempCW
      }else{
        #They are equal
        cat(algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m choosing CW\n")
        infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m choosing CW\n")
        
        
        #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
        
        boatOrder <- paste0(printOrdCW,collapse = "-")
        theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostCW, 3), round(AWDCW, 3), 
                                  nChargingCW, nBSandDepot, round(elapsedSec, 3),
                                  boatPosFileName, boatOrder, shortestPathCW, sep=", ")
        #Here color the path
        for (cnt in (1:shortestRescuePathListLenCW)) {
          E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
          E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
        }
        myGraph$value <- gTempCW
      }
      
      
      
      
      #Here color the path
      for (cnt in (1:shortestRescuePathListLenCW)) {
        E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
        E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
      }
      myGraph$value <- gTempCW
    }
    
    infoTxt <- paste0(infoTxt,"---------------------------------------------\n")
    
    
    
    
    
    #infoTxt <- paste0(infoTxt,"---------------------------------------------\n")
    
    ###################################################### STAT COMBINED ###########################################################
    # output$info<- renderUI(
    #   HTML(
    #     paste(
    #       c("<pre>", capture.output(cat(infoTxt)), "</pre>"),
    #       collapse = "<br>"
    #     )
    #   )
    # )
    
    cat(infoTxt,"\n")
    
    
    #########################################################################################
    #END: For many boat one BS
    #########################################################################################
    
    # isolate({
    #   new_zoom <- 9
    #   if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
    # })
    
    # cat("Rescue Lines are drawn.\n")
    
    
    
    cat(theRowForResults ,"\n",file=resultFile,append=T)
    cat(algo, elapsedSec, "seconds\n")
    
    cat("\n*********************************END",algo,"*****************************************\n")
  }
  ########################################################################################################
  
  #Calling wrappers for button press
  ######################################################################################################## 
  observeEvent(input$concaveTSPRedGray,{
    cat("\n Calling  concaveTSPAlgo(rg)\n")
    
    concaveTSPAlgo("rg")
  })
  
  
  observeEvent(input$concaveTSPGray,{
    cat("\n Calling  concaveTSPAlgo(g)\n")
    
    concaveTSPAlgo("g")
  })
  
  ######################################################################################################## 
  
  
  
  
  ########################################################################################################
  #END CONCAVE TSP
  ########################################################################################################
  
  
  
  
  
  
  
  
  
  
  
  ########################################################################################################
  #BEGIN BRUTE FORCE
  ########################################################################################################
  #OK uses gTemp
  #OK infotext prints the method name
  ########################################################################################################
  ########################################################################################################
  permAlgo <- function(edgeType) {
    
    
    
    #Assuming that we have the graph save local copy to work on it
    
    if (edgeType == "g"){
      algo <- "permGray"
      gTemp <- gGray
    }else if (edgeType == "rg"){
      algo <- "permRedGray"
      gTemp <- gRedGray
    }
    
    cat("\n*********************************BEGIN",algo,"*****************************************\n")
    
    tic(algo)
    
    
    cat(algo,": Permutation", nBT ,"boats with",nBSandDepot,"bs+depots.\n")
    
    currentDroneRange <- reac()$drone_range
    
    
    
    
    
    # updateRadioButtons(
    #   session = session, 
    #   inputId = "edgeFilter",
    #   selected = "g"
    # )
    
    # ################################################
    # #CC Analysis of the graph. Good for multiple BSs
    # ################################################
    # #Here check if depots are connected or isolated label them!!!
    # cl <- components(gTemp)
    # #DEBUG
    # cat("There are", cl$no, "CCs in the graph\n");
    # cclist <- lapply(seq_along(cl$csize)[cl$csize > 0], function(x) V(gTemp)$label[cl$membership %in% x])
    # 
    # #Lets find the group BS is in!!!
    # BSccId <- which("BS1" %in% V(gTemp)$label[cl$membership])
    # #May be BSccId <- cl$membership[1]
    # 
    # ################################################
    # #DEBUG
    # # cat("CClist:", unlist(cclist), "\n")
    # cat("CCs are:\n")
    # cat("--------\n")
    # for (i in (1:length(cclist))) {
    #   cat("CC",i,"--",cclist[[i]], "\n")
    # }
    # cat("\n")
    # ################################################
    
    nperm <- factorial(nBT)
    
    
    #DEDBUG
    #cat("BS:",nBS," Depots:", nDT, " Boats:", nBT, "Permutations:", nperm,"\n")
    
    #For many boats
    #DEBUG
    
    # cat("Perm Rescue for", nBT ,"boats\n")
    
    
    
    
    #########################################################################################
    #BEGIN: For many boat one BS
    #########################################################################################
    #First you need to update the adjacency mtx
    #For this you need to find chains of boat that a drone can traverse without refueling
    #Think of the algo to do this in an efficient way
    #Memory should not be a problem so find the fastest way
    #Create matrix for boat-to-boat distances
    #How about thinking of the completely connected graph of boats only
    #The goal:
    #Find the shortest path that can contain the most number of boats
    #The path length should be limited to the drone range
    #At the end of the path and at the beginning you should have depots(fueling stations)
    #So basically you find a path between two depots, "drone-range" apart
    #and in between you squeeze as many boat as you can
    #Obviously you need to consider starting with two boats that they have distance
    #less than the drone range and you can connect them to depots
    #
    
    
    #DEBUG cat("NBoat-1BS\n")
    #Clear message area 
    # my_text <-""
    # output$msg1<- renderText({  })
    
    #Here is a brute force way to find the TSP tour involving boats
    #Try all the permutations of the boats by finding sp from one to another
    #Then pick the min path!!!
    #cat("Boats:",reac()$nBoat,"\n")
    boatVids <- c((nDT + nBS + 1) : (nDT + nBS + nBT) ) # vector for vertex ids
    cat("Boat vertex ids:",boatVids,"\n")
    boatPerm <- gtools::permutations(nBT,nBT, boatVids) #permute it
    nPerm <- nrow(boatPerm) #how many permutations
    cat("Trying",nPerm,"permutations!\n")
    cat("\n************************************************************************************\n")
    shortestPathHops <- Inf #big number!
    shortestPathCost <- Inf #big number!
    shortestPath <- "" 
    shortestRescuePath <- NULL
    
    minAWDHops <- Inf #big number!
    minAWDPathCost <- Inf #big number!
    minAWDPath <- "" 
    minAWD <- Inf
    
    ################################################
    #Global list to save data from permutations
    ################################################
    #We need an array or a vector to store the 
    #rescuePaths for each Boat!!!
    #Number of charging
    #Average waiting dist stuff
    #Later we will check the intersections and stuff
    #May be cost too, otherwise we need to calculate it from the path
    
    #rescuePathList <- list()
    
    AllPermPathCostList <- list()
    AllPermPathHopsList <- list()
    AllPermAWDList  <- list()
    AllPermNChargingsList  <- list()
    AllPermPathList <- list()
    AllPermPathListLen <- 0
    ################################################
    
    
    
    #ASSUME THAT EVERYTHING IS CONNECTED!!!
    #DO NOT LEAVE ANY
    ##########################################################################################################
    for (k in (1:nPerm)){
      
      #get the vector showing the order of rescue (permutation)
      rescueOrd <- boatPerm[k,]
      
      printOrd <- sprintf("%s%d","B",(rescueOrd - (nDT + nBS)))
      
      theBoatNumPerm <- (rescueOrd - (nDT + nBS))
      #The following has to be list of list
      #Permutations and
      #For each permutation for each sp (boat to boat)
      PermPathList <- list()
      PermPathListLen <- 0
      
      totPathLen <- 0
      totPathCost <- 0
      totWD <- 0
      
      totPath <- ""
      text2print <- paste(k,"-)","Trying:",paste(printOrd,collapse = " -> "),":\n")
      
      #DEBUG
      cat("**********************************************************\n")
      cat(k,"-)","Trying:",paste(printOrd,collapse = " -> "),":\n")
      
      theResultList <- my_get_metrics_permDEBUG(gTemp,  theBoatNumPerm, currentDroneRange)
      #  theResult[[1]] : Path length in meters
      #  theResult[[2]] : Path length in hops
      #  theResult[[3]] : AVG waiting distance in meters
      #  theResult[[4]] : Number of chargings
      #  theResult[[5]] : The path string for printing
      #  theResult[[6]] : The path for coloring
      #  theResult[[7]] : The path len for coloring
      
      # theResult[[1]] <- totPathCost 
      # theResult[[2]] <- totPathLen
      # theResult[[3]] <- AWD 
      # theResult[[4]] <- nCharging
      # theResult[[5]] <- path2print
      
      #DEBUG
      cat("my_get_metrics_permDEBUG -- Drone Range is:", currentDroneRange,"m.\n")
      cat("my_get_metrics_permDEBUG -- Boat perm cost is:",theResultList[[1]],"m.",round(theResultList[[1]]/currentDroneRange,nPrecDigit),"du.\n")
      cat("my_get_metrics_permDEBUG -- Boat perm hops:",theResultList[[2]],"\n")
      cat("my_get_metrics_permDEBUG -- Boat perm AWD:",theResultList[[3]],"m.",round(theResultList[[3]]/currentDroneRange,nPrecDigit), "du.\n")
      cat("my_get_metrics_permDEBUG -- Boat perm chargings:",theResultList[[4]],"\n")
      cat("my_get_metrics_permDEBUG -- Boat perm opt path:",theResultList[[5]],"\n")
      
      
      
      ###########################################
      #Save the rescue path and info for this permutation
      ###########################################
      AllPermPathListLen <- AllPermPathListLen + 1
      AllPermPathList[[AllPermPathListLen]] <- theResultList[[6]]
      AllPermPathCostList[[AllPermPathListLen]]  <- theResultList[[1]]
      AllPermAWDList[[AllPermPathListLen]]  <- theResultList[[3]]
      AllPermNChargingsList[[AllPermPathListLen]]  <- theResultList[[4]]
      AllPermPathHopsList[[AllPermPathListLen]] <- theResultList[[2]]
      ###########################################
      
      
      
      
      
      
      #Here is the selection of the "optimum" path
      #Consider tie-break rules by adding these measures:
      # Avg waiting dist
      # The time for the first rescue
      
      if (round(theResultList[[1]]) < round(shortestPathCost)) {
        shortestPathCost <- theResultList[[1]]
        shortestPathHops <- theResultList[[2]]
        shortestPathAWD <- theResultList[[3]]
        shortestPermId <- k
        shortestPath <- theResultList[[5]]
        shortestPathId <- AllPermPathListLen
        shortestRescuePathList <- theResultList[[6]]
        shortestRescuePathListLen <- theResultList[[7]]
        optimPrintOrd <- printOrd
      } else  if (round(theResultList[[1]]) == round(shortestPathCost)) {
        if(round(theResultList[[3]]) < round(shortestPathAWD)) {
          shortestPathCost <- theResultList[[1]]
          shortestPathHops <- theResultList[[2]]
          shortestPathAWD <- theResultList[[3]]
          shortestPermId <- k
          shortestPath <- theResultList[[5]]
          shortestPathId <- AllPermPathListLen
          shortestRescuePathList <- theResultList[[6]]
          shortestRescuePathListLen <- theResultList[[7]]
          optimPrintOrd <- printOrd
          
        }
        
      }
      
      
      
      
      text2print <- paste0(text2print,
                           "The cost: ",round(theResultList[[1]],nPrecDigit),"m, hops: ", theResultList[[2]],"\n",
                           "AWD: ",round(theResultList[[3]],nPrecDigit),"m\n",
                           "Number of full chargings: ",theResultList[[4]],"\n",
                           "The Path: ", theResultList[[5]],"\n",
                           "---------------------------------------------\n")
      
      
      # my_text <- paste0(my_text, text2print)
      # output$msg1<- renderText({ my_text })
      
      
      
      
    }#For loop over permutations
    ##########################################################################################################
    
    cat("*********************************************************************************\n")
    cat(text2print)
    
    
    #Here color the path
    #DEBUG
    cat("*********************************************************************************\n")
    cat("ShortestPermId:",shortestPermId ,"--- ShortestPathId:", shortestPathId,"\n")
    cat("Shortest path list has",shortestRescuePathListLen,"parts and has",shortestPathHops,"hops.\n")
    cat("Shortest path has cost of:",shortestPathCost,"m which is",round((shortestPathCost / input$drone_range),nPrecDigit),
        "times the drone range(",input$drone_range,"m)\n")
    
    
    
    # for (cnt in (1:shortestRescuePathListLen)) {
    #   E(gTemp, path=c(shortestRescuePathList[[cnt]]))$color <- rescuePathCol
    #   E(gTemp, path=c(shortestRescuePathList[[cnt]]))$width <- 2
    # }
    # myGraph$value <- gTemp
    
    
    
    # 
    # finalShortestPath <- ""
    # #Combine the vertex labels on the optim path:
    # for (part in (1:shortestRescuePathListLen)) {
    #   finalShortestPath <- paste0(finalShortestPath, "--", paste(V(gTemp)[AllPermPathList[[shortestPermId]][[part]]$vpath[[1]]]$label, collapse = "->") )
    # }
    
    cat("Shortest path:",shortestPath,"\n")
    
    infoTxt <- paste0("---------------------------------------------\n",
                      algo,":\n",
                      "---------------------------------------------\n",
                      "BS: ",1," Depots: ", nDT, " Boats: ", nBT, " Permutations: ", factorial(nBT),"\n",
                      "Shortest rescue perm(", shortestPermId, "):",paste0(optimPrintOrd,collapse = "->"),"\n",
                      "The cost: ",round(shortestPathCost,nPrecDigit),"m, hops: ", shortestPathHops,"\n",
                      "AWD: ",round(AllPermAWDList[[shortestPermId]],nPrecDigit),"m\n",
                      "Number of full chargings: ", AllPermNChargingsList[[shortestPermId]],"\n",
                      "The Path: ",shortestPath,"\n",
                      "---------------------------------------------\n")
    
    
    # output$info<- renderUI(
    #   HTML(
    #     paste(
    #       c("<pre>", capture.output(cat(infoTxt)), "</pre>"),
    #       collapse = "<br>"
    #     )
    #   )
    # )
    
    cat(infoTxt,"\n")
    
    #########################################################################################
    #END: For many boat one BS
    #########################################################################################
    
    
    
    
    # isolate({
    #   new_zoom <- 9
    #   if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
    # })
    # 
    # cat("Rescue Lines are drawn.\n")
    elapsed <- toc()
    
    elapsedSec <- elapsed$toc - elapsed$tic
    cat(algo, elapsedSec, "seconds\n")
    
    
    
    #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
    
    boatOrder <- paste0(optimPrintOrd, collapse = "-")
    theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCost, 3), round(AllPermAWDList[[shortestPermId]], 3), 
                              AllPermNChargingsList[[shortestPermId]], nBSandDepot, round(elapsedSec, 3),
                              boatPosFileName, boatOrder, shortestPath, sep=", ")
    
    
    
    
    cat(theRowForResults ,"\n",file=resultFile,append=T)
    cat("\n*********************************END",algo ,"*****************************************\n")
  }
  ########################################################################################################
  
  #Calling wrappers for button press
  ######################################################################################################## 
  observeEvent(input$permRedGray,{
    cat("\n Calling  perm4UI(rg)\n")
    
    permforUI("rg")
  })
  
  observeEvent(input$permGray,{
    cat("\n Calling  perm4UI(g)\n")
    
    permforUI("g")
  })
  
  observeEvent(input$permRedGrayYellow,{
    cat("\n Calling  perm4UI(rgy)\n")
    
    permforUI("rgy")
  })
  
  
  ########################################################################################################  
  
  
  
  
  
  ########################################################################################################
  ########################################################################################################
  #OK uses gTemp
  #OK infotext prints the method name
  ########################################################################################################
  ########################################################################################################
  permforUI <- function(edgeType){
    
    
    #Assuming that we have the graph save local copy to work on it
    
    if (edgeType == "g"){
      algo <- "permGray"
      gTemp <- gGray
      updateRadioButtons(
        session = session, 
        inputId = "edgeFilter",
        selected = "rg"
      )
    }else if (edgeType == "rg"){
      algo <- "permRedGray"
      gTemp <- gRedGray
      updateRadioButtons(
        session = session, 
        inputId = "edgeFilter",
        selected = "g"
      )
    }else if (edgeType == "rgy"){
      algo <- "permRedGrayYellow"
      gTemp <- gRedGrayYellow
      updateRadioButtons(
        session = session, 
        inputId = "edgeFilter",
        selected = "rgy"
      )
    }
    
    cat("\n*********************************BEGIN",algo,"*****************************************\n")
    
    tic(algo)
    
    
    cat(algo,": Permutation", nBT ,"boats with",nBSandDepot,"bs+depots.\n")
    
    currentDroneRange <- reac()$drone_range
    
    
    
    ################################################
    #CC Analysis of the graph. Good for multiple BSs
    ################################################
    #Here check if depots are connected or isolated label them!!!
    cl <- components(gTemp)
    #DEBUG
    cat("There are", cl$no, "CCs in the graph\n");
    cclist <- lapply(seq_along(cl$csize)[cl$csize > 0], function(x) V(gTemp)$label[cl$membership %in% x])
    
    #Lets find the group BS is in!!!
    BSccId <- which("BS1" %in% V(gTemp)$label[cl$membership])
    #May be BSccId <- cl$membership[1]
    
    ################################################
    #DEBUG
    # cat("CClist:", unlist(cclist), "\n")
    cat("CCs are:\n")
    cat("--------\n")
    for (i in (1:length(cclist))) {
      cat("CC",i,"--",cclist[[i]], "\n")
    }
    cat("\n")
    ################################################
    
    nperm <- factorial(nBT)
    #DEDBUG
    cat("BS:",nBS," Depots:", nDT, " Boats:", nBT, "Permutations:", nperm,"\n")
    
   
    
    
    
    
    #########################################################################################
    #BEGIN: For many boat one BS
    #########################################################################################
    #First you need to update the adjacency mtx
    #For this you need to find chains of boat that a drone can traverse without refueling
    #Think of the algo to do this in an efficient way
    #Memory should not be a problem so find the fastest way
    #Create matrix for boat-to-boat distances
    #How about thinking of the completely connected graph of boats only
    #The goal:
    #Find the shortest path that can contain the most number of boats
    #The path length should be limited to the drone range
    #At the end of the path and at the beginning you should have depots(fueling stations)
    #So basically you find a path between two depots, "drone-range" apart
    #and in between you squeeze as many boat as you can
    #Obviously you need to consider starting with two boats that they have distance
    #less than the drone range and you can connect them to depots
    #
    
    
    #DEBUG cat("NBoat-1BS\n")
    #Clear message area 
    my_text <-""
    output$msg1<- renderText({  })
    
    #Here is a brute force way to find the TSP tour involving boats
    #Try all the permutations of the boats by finding sp from one to another
    #Then pick the min path!!!
    #cat("Boats:",reac()$nBoat,"\n")
    boatVids <- c((nDT + nBS + 1) : (nDT + nBS + nBT) ) # vector for vertex ids
    cat("Boat vertex ids:",boatVids,"\n")
    boatPerm <- gtools::permutations(nBT,nBT, boatVids) #permute it
    nPerm <- nrow(boatPerm) #how many permutations
    cat("Trying",nPerm,"permutations!\n")
    cat("\n************************************************************************************\n")
    shortestPathHops <- Inf #big number!
    shortestPathCost <- Inf #big number!
    shortestPath <- "" 
    shortestRescuePath <- NULL
    
    minAWDHops <- Inf #big number!
    minAWDPathCost <- Inf #big number!
    minAWDPath <- "" 
    minAWD <- Inf
    
    ################################################
    #Global list to save data from permutations
    ################################################
    #We need an array or a vector to store the 
    #rescuePaths for each Boat!!!
    #Number of charging
    #Average waiting dist stuff
    #Later we will check the intersections and stuff
    #May be cost too, otherwise we need to calculate it from the path
    
    #rescuePathList <- list()
    
    AllPermPathCostList <- list()
    AllPermPathHopsList <- list()
    AllPermAWDList  <- list()
    AllPermNChargingsList  <- list()
    AllPermPathList <- list()
    AllPermPathListLen <- 0
    ################################################
    
    
    
    #ASSUME THAT EVERYTHING IS CONNECTED!!!
    #DO NOT LEAVE ANY
    ##########################################################################################################
    for (k in (1:nPerm)){
      
      #get the vector showing the order of rescue (permutation)
      rescueOrd <- boatPerm[k,]
      
      printOrd <- sprintf("%s%d","B",(rescueOrd - (nDT + nBS)))
      
      theBoatNumPerm <- (rescueOrd - (nDT + nBS))
      #The following has to be list of list
      #Permutations and
      #For each permutation for each sp (boat to boat)
      PermPathList <- list()
      PermPathListLen <- 0
      
      totPathLen <- 0
      totPathCost <- 0
      totWD <- 0
      
      totPath <- ""
      text2print <- paste(k,"-)","Trying:",paste(printOrd,collapse = " -> "),":\n")
      
      #DEBUG
      cat("**********************************************************\n")
      cat(k,"-)","Trying:",paste(printOrd,collapse = " -> "),":\n")
      
      theResultList <- my_get_metrics_permDEBUG(gTemp,  theBoatNumPerm, currentDroneRange)
      #  theResult[[1]] : Path length in meters
      #  theResult[[2]] : Path length in hops
      #  theResult[[3]] : AVG waiting distance in meters
      #  theResult[[4]] : Number of chargings
      #  theResult[[5]] : The path string for printing
      #  theResult[[6]] : The path for coloring
      #  theResult[[7]] : The path len for coloring
      
      # theResult[[1]] <- totPathCost 
      # theResult[[2]] <- totPathLen
      # theResult[[3]] <- AWD 
      # theResult[[4]] <- nCharging
      # theResult[[5]] <- path2print
      
      #DEBUG
      cat("my_get_metrics_permDEBUG -- Drone Range is:", currentDroneRange,"m.\n")
      cat("my_get_metrics_permDEBUG -- Boat perm cost is:",theResultList[[1]],"m.",round(theResultList[[1]]/currentDroneRange,nPrecDigit),"du.\n")
      cat("my_get_metrics_permDEBUG -- Boat perm hops:",theResultList[[2]],"\n")
      cat("my_get_metrics_permDEBUG -- Boat perm AWD:",theResultList[[3]],"m.",round(theResultList[[3]]/currentDroneRange,nPrecDigit), "du.\n")
      cat("my_get_metrics_permDEBUG -- Boat perm chargings:",theResultList[[4]],"\n")
      cat("my_get_metrics_permDEBUG -- Boat perm opt path:",theResultList[[5]],"\n")
      
      
      
      ###########################################
      #Save the rescue path and info for this permutation
      ###########################################
      AllPermPathListLen <- AllPermPathListLen + 1
      AllPermPathList[[AllPermPathListLen]] <- theResultList[[6]]
      AllPermPathCostList[[AllPermPathListLen]]  <- theResultList[[1]]
      AllPermAWDList[[AllPermPathListLen]]  <- theResultList[[3]]
      AllPermNChargingsList[[AllPermPathListLen]]  <- theResultList[[4]]
      AllPermPathHopsList[[AllPermPathListLen]] <- theResultList[[2]]
      ###########################################
      
      
      
      
      
      
      #Here is the selection of the "optimum" path
      #Consider tie-break rules by adding these measures:
      # Avg waiting dist
      # The time for the first rescue
      
      # if (theResultList[[1]] < shortestPathCost) {
      #   shortestPathCost <- theResultList[[1]]
      #   shortestPathHops <- theResultList[[2]]
      #   shortestPermId <- k
      #   shortestPath <- theResultList[[5]]
      #   shortestPathId <- AllPermPathListLen
      #   shortestRescuePathList <- theResultList[[6]]
      #   shortestRescuePathListLen <- theResultList[[7]]
      #   optimPrintOrd <- printOrd
      # }
      
      if (round(theResultList[[1]]) < round(shortestPathCost)) {
        shortestPathCost <- theResultList[[1]]
        shortestPathHops <- theResultList[[2]]
        shortestPathAWD <- theResultList[[3]]
        shortestPermId <- k
        shortestPath <- theResultList[[5]]
        shortestPathId <- AllPermPathListLen
        shortestRescuePathList <- theResultList[[6]]
        shortestRescuePathListLen <- theResultList[[7]]
        optimPrintOrd <- printOrd
      } else  if (round(theResultList[[1]]) == round(shortestPathCost)) {
        if(round(theResultList[[3]]) < round(shortestPathAWD)) {
          shortestPathCost <- theResultList[[1]]
          shortestPathHops <- theResultList[[2]]
          shortestPathAWD <- theResultList[[3]]
          shortestPermId <- k
          shortestPath <- theResultList[[5]]
          shortestPathId <- AllPermPathListLen
          shortestRescuePathList <- theResultList[[6]]
          shortestRescuePathListLen <- theResultList[[7]]
          optimPrintOrd <- printOrd
          
        }
        
      }
      
      
      
      
      text2print <- paste0(text2print,
                           "The cost: ",round(theResultList[[1]],nPrecDigit),"m, hops: ", theResultList[[2]],"\n",
                           "AWD: ",round(theResultList[[3]],nPrecDigit),"m\n",
                           "Number of full chargings: ",theResultList[[4]],"\n",
                           "The Path: ", theResultList[[5]],"\n",
                           "---------------------------------------------\n")
      
      
      my_text <- paste0(my_text, text2print)
      output$msg1<- renderText({ my_text })
      
      
      
      
    }#For loop over permutations
    ##########################################################################################################
    
    cat("*********************************************************************************\n")
    cat(text2print)
    
    
    #Here color the path
    #DEBUG
    cat("*********************************************************************************\n")
    cat("ShortestPermId:",shortestPermId ,"--- ShortestPathId:", shortestPathId,"\n")
    cat("Shortest path list has",shortestRescuePathListLen,"parts and has",shortestPathHops,"hops.\n")
    cat("Shortest path has cost of:",shortestPathCost,"m which is",round((shortestPathCost / input$drone_range),nPrecDigit),
        "times the drone range(",input$drone_range,"m)\n")
    for (cnt in (1:shortestRescuePathListLen)) {
      E(gTemp, path=c(shortestRescuePathList[[cnt]]))$color <- rescuePathCol
      E(gTemp, path=c(shortestRescuePathList[[cnt]]))$width <- 2
    }
    myGraph$value <- gTemp
    
    
    
    # 
    # finalShortestPath <- ""
    # #Combine the vertex labels on the optim path:
    # for (part in (1:shortestRescuePathListLen)) {
    #   finalShortestPath <- paste0(finalShortestPath, "--", paste(V(gTemp)[AllPermPathList[[shortestPermId]][[part]]$vpath[[1]]]$label, collapse = "->") )
    # }
    
    cat("Shortest path:",shortestPath,"\n")
    
    infoTxt <- paste0("---------------------------------------------\n",
                       algo,":\n",
                      "---------------------------------------------\n",
                      "BS: ",1," Depots: ", nDT, " Boats: ", nBT, " Permutations: ", factorial(nBT),"\n",
                      "Shortest rescue perm(", shortestPermId, "):",paste0(optimPrintOrd,collapse = "->"),"\n",
                      "The cost: ",round(shortestPathCost,nPrecDigit),"m, hops: ", shortestPathHops,"\n",
                      "AWD: ",round(AllPermAWDList[[shortestPermId]],nPrecDigit),"m\n",
                      "Number of full chargings: ", AllPermNChargingsList[[shortestPermId]],"\n",
                      "The Path: ",shortestPath,"\n",
                      "---------------------------------------------\n")
    
    
    output$info<- renderUI(
      HTML(
        paste(
          c("<pre>", capture.output(cat(infoTxt)), "</pre>"),
          collapse = "<br>"
        )
      )
    )
    
    cat(infoTxt,"\n")
    
    #########################################################################################
    #END: For many boat one BS
    #########################################################################################
    
    elapsed <- toc()
    
    elapsedSec <- elapsed$toc - elapsed$tic
    cat(algo, elapsedSec, "seconds\n")
    
    
    isolate({
      new_zoom <- 9
      if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
    })
    
    cat("Rescue Lines are drawn.\n")
    cat("\n*********************************END",algo ,"*****************************************\n")
  }
  ########################################################################################################
  
  
  
  ########################################################################################################
  ########################################################################################################
  #OK uses gTemp
  #OK infotext prints the method name
  ########################################################################################################
  ########################################################################################################
  observeEvent(input$permRedGrayx,{
    
    cat("\n*********************************BEGIN permRedGray*****************************************\n")
    tic("permRedGray")
    #TODO: Red-gray paths are not checked!!!
    #Between two boats we need to check if there is a red-gray edge
    
    #Boats should be added to mtx when the user clicks
    #Here just find the sp and plot it!!!
    
    cat("permRedGray: Permutation", nBT ,"boats with",nBSandDepot,"bs+depots.\n")
    
    currentDroneRange <- reac()$drone_range
    
    
    #Assuming that we have the graph save local copy to work on it
    gTemp <- gRedGray
    updateRadioButtons(
      session = session, 
      inputId = "edgeFilter",
      selected = "rg"
    )
    
    ################################################
    #CC Analysis of the graph. Good for multiple BSs
    ################################################
    #Here check if depots are connected or isolated label them!!!
    cl <- components(gTemp)
    #DEBUG
    cat("There are", cl$no, "CCs in the graph\n");
    cclist <- lapply(seq_along(cl$csize)[cl$csize > 0], function(x) V(gTemp)$label[cl$membership %in% x])
    
    #Lets find the group BS is in!!!
    BSccId <- which("BS1" %in% V(gTemp)$label[cl$membership])
    #May be BSccId <- cl$membership[1]
    
    ################################################
    #DEBUG
    # cat("CClist:", unlist(cclist), "\n")
    cat("CCs are:\n")
    cat("--------\n")
    for (i in (1:length(cclist))) {
      cat("CC",i,"--",cclist[[i]], "\n")
    }
    cat("\n")
    ################################################
    
    nperm <- factorial(nBT)
    #DEDBUG
    cat("BS:",nBS," Depots:", nDT, " Boats:", nBT, "Permutations:", nperm,"\n")
    
    #For many boats
    #DEBUG
    
    cat("Perm Rescue for", nBT ,"boats\n")
    
    
    
    
    #########################################################################################
    #BEGIN: For many boat one BS
    #########################################################################################
    #First you need to update the adjacency mtx
    #For this you need to find chains of boat that a drone can traverse without refueling
    #Think of the algo to do this in an efficient way
    #Memory should not be a problem so find the fastest way
    #Create matrix for boat-to-boat distances
    #How about thinking of the completely connected graph of boats only
    #The goal:
    #Find the shortest path that can contain the most number of boats
    #The path length should be limited to the drone range
    #At the end of the path and at the beginning you should have depots(fueling stations)
    #So basically you find a path between two depots, "drone-range" apart
    #and in between you squeeze as many boat as you can
    #Obviously you need to consider starting with two boats that they have distance
    #less than the drone range and you can connect them to depots
    #
    
    
    #DEBUG cat("NBoat-1BS\n")
    #Clear message area 
    my_text <-""
    output$msg1<- renderText({  })
    
    #Here is a brute force way to find the TSP tour involving boats
    #Try all the permutations of the boats by finding sp from one to another
    #Then pick the min path!!!
    #cat("Boats:",reac()$nBoat,"\n")
    boatVids <- c((nDT + nBS + 1) : (nDT + nBS + nBT) ) # vector for vertex ids
    cat("Boat vertex ids:",boatVids,"\n")
    boatPerm <- gtools::permutations(nBT,nBT, boatVids) #permute it
    nPerm <- nrow(boatPerm) #how many permutations
    cat("Trying",nPerm,"permutations!\n")
    cat("\n************************************************************************************\n")
    shortestPathHops <- Inf #big number!
    shortestPathCost <- Inf #big number!
    shortestPath <- "" 
    shortestRescuePath <- NULL
    
    minAWDHops <- Inf #big number!
    minAWDPathCost <- Inf #big number!
    minAWDPath <- "" 
    minAWD <- Inf
    
    ################################################
    #Global list to save data from permutations
    ################################################
    #We need an array or a vector to store the 
    #rescuePaths for each Boat!!!
    #Number of charging
    #Average waiting dist stuff
    #Later we will check the intersections and stuff
    #May be cost too, otherwise we need to calculate it from the path
    
    #rescuePathList <- list()
    
    AllPermPathCostList <- list()
    AllPermPathHopsList <- list()
    AllPermAWDList  <- list()
    AllPermNChargingsList  <- list()
    AllPermPathList <- list()
    AllPermPathListLen <- 0
    ################################################
    
    
    
    #ASSUME THAT EVERYTHING IS CONNECTED!!!
    #DO NOT LEAVE ANY
    ##########################################################################################################
    for (k in (1:nPerm)){
      
      #get the vector showing the order of rescue (permutation)
      rescueOrd <- boatPerm[k,]
      
      printOrd <- sprintf("%s%d","B",(rescueOrd - (nDT + nBS)))
      
      theBoatNumPerm <- (rescueOrd - (nDT + nBS))
      #The following has to be list of list
      #Permutations and
      #For each permutation for each sp (boat to boat)
      PermPathList <- list()
      PermPathListLen <- 0
      
      totPathLen <- 0
      totPathCost <- 0
      totWD <- 0
      
      totPath <- ""
      text2print <- paste(k,"-)","Trying:",paste(printOrd,collapse = " -> "),":\n")
      
      #DEBUG
      cat("**********************************************************\n")
      cat(k,"-)","Trying:",paste(printOrd,collapse = " -> "),":\n")
      
      theResultList <- my_get_metrics_permDEBUG(gTemp,  theBoatNumPerm, currentDroneRange)
      #  theResult[[1]] : Path length in meters
      #  theResult[[2]] : Path length in hops
      #  theResult[[3]] : AVG waiting distance in meters
      #  theResult[[4]] : Number of chargings
      #  theResult[[5]] : The path string for printing
      #  theResult[[6]] : The path for coloring
      #  theResult[[7]] : The path len for coloring
      
      # theResult[[1]] <- totPathCost 
      # theResult[[2]] <- totPathLen
      # theResult[[3]] <- AWD 
      # theResult[[4]] <- nCharging
      # theResult[[5]] <- path2print
      
      #DEBUG
      cat("my_get_metrics_permDEBUG -- Drone Range is:", currentDroneRange,"m.\n")
      cat("my_get_metrics_permDEBUG -- Boat perm cost is:",theResultList[[1]],"m.",round(theResultList[[1]]/currentDroneRange,nPrecDigit),"du.\n")
      cat("my_get_metrics_permDEBUG -- Boat perm hops:",theResultList[[2]],"\n")
      cat("my_get_metrics_permDEBUG -- Boat perm AWD:",theResultList[[3]],"m.",round(theResultList[[3]]/currentDroneRange,nPrecDigit), "du.\n")
      cat("my_get_metrics_permDEBUG -- Boat perm chargings:",theResultList[[4]],"\n")
      cat("my_get_metrics_permDEBUG -- Boat perm opt path:",theResultList[[5]],"\n")
      
      
      
      ###########################################
      #Save the rescue path and info for this permutation
      ###########################################
      AllPermPathListLen <- AllPermPathListLen + 1
      AllPermPathList[[AllPermPathListLen]] <- theResultList[[6]]
      AllPermPathCostList[[AllPermPathListLen]]  <- theResultList[[1]]
      AllPermAWDList[[AllPermPathListLen]]  <- theResultList[[3]]
      AllPermNChargingsList[[AllPermPathListLen]]  <- theResultList[[4]]
      AllPermPathHopsList[[AllPermPathListLen]] <- theResultList[[2]]
      ###########################################
      
      
      
      
      
      
      #Here is the selection of the "optimum" path
      #Consider tie-break rules by adding these measures:
      # Avg waiting dist
      # The time for the first rescue
      
      if (theResultList[[1]] < shortestPathCost) {
        shortestPathCost <- theResultList[[1]]
        shortestPathHops <- theResultList[[2]]
        shortestPermId <- k
        shortestPath <- theResultList[[5]]
        shortestPathId <- AllPermPathListLen
        shortestRescuePathList <- theResultList[[6]]
        shortestRescuePathListLen <- theResultList[[7]]
        optimPrintOrd <- printOrd
      }
      
      
      
      
      text2print <- paste0(text2print,
                           "The cost: ",round(theResultList[[1]],nPrecDigit),"m, hops: ", theResultList[[2]],"\n",
                           "AWD: ",round(theResultList[[3]],nPrecDigit),"m\n",
                           "Number of full chargings: ",theResultList[[4]],"\n",
                           "The Path: ", theResultList[[5]],"\n",
                           "---------------------------------------------\n")
      
      
      my_text <- paste0(my_text, text2print)
      output$msg1<- renderText({ my_text })
      
      
      
      
    }#For loop over permutations
    ##########################################################################################################
    
    cat("*********************************************************************************\n")
    cat(text2print)
    
    
    #Here color the path
    #DEBUG
    cat("*********************************************************************************\n")
    cat("ShortestPermId:",shortestPermId ,"--- ShortestPathId:", shortestPathId,"\n")
    cat("Shortest path list has",shortestRescuePathListLen,"parts and has",shortestPathHops,"hops.\n")
    cat("Shortest path has cost of:",shortestPathCost,"m which is",round((shortestPathCost / input$drone_range),nPrecDigit),
        "times the drone range(",input$drone_range,"m)\n")
    for (cnt in (1:shortestRescuePathListLen)) {
      E(gTemp, path=c(shortestRescuePathList[[cnt]]))$color <- rescuePathCol
      E(gTemp, path=c(shortestRescuePathList[[cnt]]))$width <- 2
    }
    myGraph$value <- gTemp
    
    
    
    # 
    # finalShortestPath <- ""
    # #Combine the vertex labels on the optim path:
    # for (part in (1:shortestRescuePathListLen)) {
    #   finalShortestPath <- paste0(finalShortestPath, "--", paste(V(gTemp)[AllPermPathList[[shortestPermId]][[part]]$vpath[[1]]]$label, collapse = "->") )
    # }
    
    cat("Shortest path:",shortestPath,"\n")
    
    infoTxt <- paste0("---------------------------------------------\n",
                      "Brute-force Red-Gray:\n",
                      "---------------------------------------------\n",
                      "BS: ",1," Depots: ", nDT, " Boats: ", nBT, " Permutations: ", factorial(nBT),"\n",
                      "Shortest rescue perm(", shortestPermId, "):",paste0(optimPrintOrd,collapse = "->"),"\n",
                      "The cost: ",round(shortestPathCost,nPrecDigit),"m, hops: ", shortestPathHops,"\n",
                      "AWD: ",round(AllPermAWDList[[shortestPermId]],nPrecDigit),"m\n",
                      "Number of full chargings: ", AllPermNChargingsList[[shortestPermId]],"\n",
                      "The Path: ",shortestPath,"\n",
                      "---------------------------------------------\n")
    
    
    output$info<- renderUI(
      HTML(
        paste(
          c("<pre>", capture.output(cat(infoTxt)), "</pre>"),
          collapse = "<br>"
        )
      )
    )
    
    cat(infoTxt,"\n")
    
    #########################################################################################
    #END: For many boat one BS
    #########################################################################################
    
    
    
    
    isolate({
      new_zoom <- 9
      if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
    })
    
    cat("Rescue Lines are drawn.\n")
    toc()
    cat("\n*********************************END permRedGray*****************************************\n")
  })
  ########################################################################################################
  
  
  ########################################################################################################
  #END BRUTE FORCE
  ########################################################################################################
  
  
  
  
  
  
  
  
  
  
  
  #########################################################################################################
  #BEGIN GA BASED
  #########################################################################################################
  #OK  uses gTemp
  #########################################################################################################
  #########################################################################################################
  observeEvent(input$gaPermRedGray,{ 
    #########################################################################################################
    
    cat("\n*********************************BEGIN gaPermRG*****************************************\n")
    tic("gaPermRG")
    #########################################################################################################
    
    cat("gaPermRedGray:", nBT ,"boats with",nBSandDepot,"bs+depots.\n")
    
    gTemp <- gRedGray
    
    #costOfPerm <- my_get_cost_permDEBUG(gTempCW, boatPermCW, 1.5, 1.0, 1.0, currentDroneRange)
    minBoatVec <- rep(1,nBT)
    maxBoatVec <- rep(nBT, nBT)
    
    # GA.fit <- ga(type = "permutation", fitness = tpsFitness, distMatrix = D, min = 1, 
    #              max = attr(eurodist, "Size"), popSize = 10, maxiter = 500, run = 100, pmutation = 0.2, 
    #              monitor = NULL)
    
    # GA <- ga(type = "real-valued",
    #          fitness = function(xxx) fit_with_weights(xxx, wCoverage=wCoveragex, wOverflow=wOverflowx, wOverlap=wOverlapx, wDist=wDistx, wScore=xwScore),
    #          lower = rep(c(1,1,rmin),ndronesVal), upper = rep(c(w,h,rmax),ndronesVal), parallel = TRUE,
    #          # popSize = populationSize, maxiter = input$MaxIteration, optim = TRUE,
    #          popSize = populationSize, run = 20, maxiter = MaxIterationx, optim = TRUE,
    #          suggestions = InitialsolSuggestion)
    # 
    # function(gx, boatPerm, plw, awtw, ncw, currentDroneRange) 
    
    
    
    drange = input$drone_range
    optimPerm <- ga(type="permutation", 
                    fitness=function(xxx) my_get_cost_perm(gx = gTemp, xxx , distw = distWeight, plw =  plWeight, 
                                                           awtw =  awgWaitingWeight, ncw =  numChargingWeight, 
                                                           currentDroneRange = drange),
                    lower=minBoatVec, 
                    upper=maxBoatVec, 
                    popSize = 100,
                    run = 20,
                    maxiter = 100 * ceiling(nBT/10), #Give 100 iters per 10 boats???????????????????
                    optim = TRUE
    )
    
    bestPerm <- summary(optimPerm)$solution
    #THERE CAN BE MULTIPLE SOLUTIONS TIE BREAKING MAYBE REQUIRED!!!!
    #Take the first soln:
    bestPerm <- as.vector(bestPerm[1,])
    
    rescueOrd <- bestPerm + (nDT + nBS)
    
    printOrd <- sprintf("%s%d","B",(rescueOrd - (nDT + nBS)))
    
    
    bestCost <- summary(optimPerm)$fitness
    
    
    
    
    
    costOfPerm <- my_get_cost_permDEBUG(gTemp, bestPerm, distWeight, plWeight, awgWaitingWeight, numChargingWeight, drange)
    
    cat("Weighted boat perm cost is:",costOfPerm,"\n")
    cat("Perm is:", bestPerm, "<-->",rescueOrd, "<-->", printOrd,"The cost is:",bestCost,"\n")
    #########################################################################################################
    toc()
    #########################################################################################################
    
    cat("\n*********************************END gaPermRG*****************************************\n")
  })
  #########################################################################################################
  
  #########################################################################################################
  #END GA BASED
  #########################################################################################################
  
  
  
  
  
  
  
  
  
  #########################################################################################################
  #BEGIN CLOOK BASED
  #########################################################################################################
  #OK uses gTempACW and gTempCW
  #OK AWD
  #OK Number of chargings
  #OK Red edge to BS from the last Boat without going to gray edge
  #########################################################################################################
  #########################################################################################################
  clookAlgo <- function(edgeType) { 
    
    
    
    #Assuming that we have the graph save local copy to work on it
    #Both for CW and ACW
    if (edgeType == "g"){
      algo <- "clookGray"
      gTempCW <- gGray
      gTempACW <- gGray
    }else if (edgeType == "rg"){
      algo <- "clookRedGray"
      gTempCW <- gRedGray
      gTempACW <- gRedGray
    }
    
    cat("\n*********************************BEGIN",algo,"*****************************************\n")
    
    tic(algo)
    #TODO: Red-gray paths are not checked!!!
    #Between two boats we need to check if there is a red-gray edge
    
    #Boats should be added to mtx when the user clicks
    #Here just find the sp and plot it!!!
    
    cat(algo,":", nBT ,"boats with",nBSandDepot,"bs+depots.\n")
    
    currentDroneRange <- reac()$drone_range
    
    
    
    
    # updateRadioButtons(
    #   session = session, 
    #   inputId = "edgeFilter",
    #   selected = "rg"
    # )
    # 
    # 
    # ################################################
    # #CC Analysis of the graph. Good for multiple BSs
    # #Does not matter which gTemp
    # #Initially they are same!!!!
    # ################################################
    # #Here check if depots are connected or isolated label them!!!
    # cl <- components(gTempCW)
    # #DEBUG
    # cat("There are", cl$no, "CCs in the graph\n");
    # cclist <- lapply(seq_along(cl$csize)[cl$csize > 0], function(x) V(gTempCW)$label[cl$membership %in% x])
    # 
    # #Lets find the group BS is in!!!
    # BSccId <- which("BS1" %in% V(gTempCW)$label[cl$membership])
    # #May be BSccId <- cl$membership[1]
    # 
    # ################################################
    # #DEBUG
    # # cat("CClist:", unlist(cclist), "\n")
    # cat("CCs are:\n")
    # cat("--------\n")
    # for (i in (1:length(cclist))) {
    #   cat("CC",i,"--",cclist[[i]], "\n")
    # }
    # cat("\n")
    # ################################################
    
    
    #Convert BTMtx to dataframe and sort the points clockwise
    BTDF <- as.data.frame(BTMtx)
    colnames(BTDF)<- c("lon","lat")
    #Add column for original boat numbers
    BTDF$BNo <- seq.int(nrow(BTDF))
    BTDF$DistToBS <- apply(BTMtx, 1, function(x) distGeo(x, BSMtx[1,]))
    sortedBTMtx <- my_sort_points(BTDF, y="lat", x="lon", bsx=BSMtx[1,1], bsy=BSMtx[1,2])
    
    BTGroups <- split(sortedBTMtx, cut(sortedBTMtx$angle_degrees, breaks = sectors, labels = seq(1:(nSectors-1))), drop = TRUE)
    nBTGroups <- length(BTGroups)
    
    #DEBUG
    cat("---------------------------------------------------------------------------------------------\n")
    cat("Sorted boats according to angle:\n")
    cat("---------------------------------------------------------------------------------------------\n")
    print(sortedBTMtx)
    cat("---------------------------------------------------------------------------------------------\n")
    cat(nBTGroups, "Groups according to angle sectors and inside each group sorted according to Dist to BS:\n")
    cat("Sector groups:",sectors,"\n")
    cat("---------------------------------------------------------------------------------------------\n")
    print(BTGroups)
    cat("---------------------------------------------------------------------------------------------\n")
    
    # 
    # 
    # 
    # 
    # nchunks <- ceiling( 
    #   (
    #     ceiling(sign(max(sortedBTMtx$angle_degrees)) * max(sortedBTMtx$angle_degrees)) - 
    #     (sign(min(sortedBTMtx$angle_degrees)) * ceiling(sign(min(sortedBTMtx$angle_degrees)) * min(sortedBTMtx$angle_degrees)))
    #   )
    #   / BTGroupSectorAngle) + 1
    
    
    
    if (nBTGroups > 1) {
      
      #We have many sector groups!!!!
      
      #DEBUG
      cat(algo,": ",nBTGroups,"BT sector groups.\n")
      
      DistToBSSortedPermCW <- NULL
      DistToBSSortedPermACW <- NULL
      
      DistToBSSortedPermCWlist <- list()
      DistToBSSortedPermACWlist <- list()
      
      #HERE MODIFY THE ALGO - add the "dynamic direction" twist!!!
      #Conservative up-down scanning:
      #It seems when scan changes sector you need to find the closest boat from the next sector to go
      # You need to consider the top and bottom boats in the sector
      #So there will be no fixed direction
      #Basically when you go to the next sector you jump to the closest boat or GrayDepot and then scan the others
      # So if the closest boat (or depot!!!) is the top(furthest) one then direction will be down(towards center or towards the BS)
      #if the closest boat is the bottom(closest to BS) one then the direction will be up (away from the BS)
      
      
      
      #HERE THERE IS A BUG IN DIRS
      #TEST WITH TWO GROUPS
      
      ####################################################################################
      #Loop for CW
      #The first sector should be away from BS and the last one should be towards BS!!!
      ####################################################################################
      ScanDir <- 1 #up Away from BS
      
      for (btx in (nBTGroups:1)) {
        
        groupDF <- as.data.frame(BTGroups[btx])
        colnames(groupDF) <- colnames(sortedBTMtx)
        
        if (ScanDir == 1) {
          #Away from BS
          sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = FALSE),] 
          ScanDir <- 0 #Change to Down to BS
        }else{
          #Towards BS
          sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
          ScanDir <- 1 #Change to Up again away from BS
        }
        
        #For CW the last group, "1", should be towards BS
        if (btx == 1) {
          sortedGroupDFX <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
          DistToBSSortedPermCW <- c(DistToBSSortedPermCW, sortedGroupDFX$BNo)
          DistToBSSortedPermCWlist[[nBTGroups - btx + 1]] <- sortedGroupDFX
        }else{
          DistToBSSortedPermCW <- c(DistToBSSortedPermCW, sortedGroupDF$BNo)
          DistToBSSortedPermCWlist[[nBTGroups - btx + 1]] <- sortedGroupDF
        }
      }#end for loop on nBTGroups
      
      
      
      
      
      ####################################################################################
      #Loop for ACW
      #The first sector should be away from BS and the last one should be towards BS!!!
      ####################################################################################
      ScanDir <- 1 #up Away from BS
      
      for (btx in (1:nBTGroups)) {
        
        groupDF <- as.data.frame(BTGroups[btx])
        colnames(groupDF) <- colnames(sortedBTMtx)
        
        if (ScanDir == 1) {
          #Away from BS
          sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = FALSE),] 
          ScanDir <- 0 #Change to Down to BS
        }else{
          #Towards BS
          sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
          ScanDir <- 1 #Change to Up again away from BS
        }
        
        #For ACW the last group, "nBTGroups", should be towards BS
        if (btx == nBTGroups) {
          sortedGroupDFX <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
          DistToBSSortedPermACW <- c(DistToBSSortedPermACW, sortedGroupDFX$BNo)
          DistToBSSortedPermACWlist[[btx]] <- sortedGroupDFX
        }else{
          DistToBSSortedPermACW <- c(DistToBSSortedPermACW, sortedGroupDF$BNo)
          DistToBSSortedPermACWlist[[btx]] <- sortedGroupDF
        }
      }#end for loop on nBTGroups
      
    }else{
      #When we have a single sector of boats!!!!
      #DEBUG
      cat(algo,": ",nBTGroups,"BT sector groups.\n")
      
      DistToBSSortedPermCW <- NULL
      DistToBSSortedPermACW <- NULL
      
      ScanDir <- 1 #up
      groupDF <- as.data.frame(BTGroups)
      colnames(groupDF) <- colnames(sortedBTMtx)
      if (ScanDir == 1) {
        sortedGroupDF <- groupDF[order(groupDF$DistToBS),] 
        ScanDir <- 0 #Down
      }else{
        sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
        ScanDir <- 1 #Up again
      }
      DistToBSSortedPermCW <- c(DistToBSSortedPermCW, sortedGroupDF$BNo)
      DistToBSSortedPermACW <- c(sortedGroupDF$BNo, DistToBSSortedPermACW)
    }
    
    nperm <- 1
    
    #DEBUG
    cat("BS:",nBS," Depots:", nDT, " Boats:", nBT, "Trying Permutation:", sortedBTMtx$BNo,"CW sorted perm:",DistToBSSortedPermCW ,"\n")
    cat("BS:",nBS," Depots:", nDT, " Boats:", nBT, "Trying Permutation:", sortedBTMtx$BNo,"ACW sorted perm:",DistToBSSortedPermACW ,"\n")
    
    
    infoTxt <- paste0("---------------------------------------------\n",
                      algo,":\n",
                      "---------------------------------------------\n",
                      "BS: ",1," Depots: ", nDT, " Boats: ", nBT, " Permutations: ", factorial(nBT),"\n",
                      "---------------------------------------------\n")
    
    
    
    
    
    ###################################################### BEGIN CLOCKWISE ############################################################
    
    #DEBUG
    cat("\n*********************************BEGIN CLOCKWISE*****************************************\n")
    
    
    ################################################
    #We need an array or a vector to store the rescuePaths for each Boat!!!
    #Later we will check the intersections and stuff
    
    rescuePathList <- list()
    ################################################
    
    #########################################################################################
    #BEGIN: For many boat one BS
    #########################################################################################
    #First you need to update the adjacency mtx
    #For this you need to find chains of boat that a drone can traverse without refueling
    #Think of the algo to do this in an efficient way
    #Memory should not be a problem so find the fastest way
    #Create matrix for boat-to-boat distances
    #How about thinking of the completely connected graph of boats only
    #The goal:
    #Find the shortest path that can contain the most number of boats
    #The path length should be limited to the drone range
    #At the end of the path and at the beginning you should have depots(fueling stations)
    #So basically you find a path between two depots, "drone-range" apart
    #and in between you squeeze as many boat as you can
    #Obviously you need to consider starting with two boats that they have distance
    #less than the drone range and you can connect them to depots
    #
    
    
    #DEBUG cat("NBoat-1BS\n")
    #Clear message area 
    # my_text <-""
    # output$msg1<- renderText({  })
    
    #Here is a brute force way to find the TSP tour involving boats
    #Try all the permutations of the boats by finding sp from one to another
    #Then pick the min path!!!
    #cat("Boats:",reac()$nBoat,"\n")
    boatVids <- c((nDT + nBS + 1) : (nDT + nBS + nBT) ) # vector for vertex ids
    cat("CW - Boat vertex ids:",boatVids,"\n")
    boatPermCW <- DistToBSSortedPermCW
    cat("CW - Boat perm vertex ids:",boatPermCW,"\n")
    
    
    #DEBUG
    #costOfPerm <- my_get_cost_permDEBUG(gTempCW, boatPermCW, distWeight, plWeight, awgWaitingWeight, numChargingWeight, currentDroneRange)
    #cat("my_get_cost_permDEBUG -- Weighted boat perm cost is:",costOfPerm,"\n")
    
    
    
    ###################################################### STAT CLOCKWISE ###########################################################
    theResultList <- my_get_metrics_permDEBUG(gTempCW, boatPermCW, currentDroneRange)
    # theResult[[1]] <- totPathCost 
    # theResult[[2]] <- totPathLen
    # theResult[[3]] <- AWD 
    # theResult[[4]] <- nCharging
    # theResult[[5]] <- path2print
    
    #DEBUG
    cat("CW my_get_metrics_permDEBUG -- Drone Range is:", currentDroneRange,"m.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm cost is:",theResultList[[1]],"m.",round(theResultList[[1]]/currentDroneRange,nPrecDigit),"du.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm hops:",theResultList[[2]],"\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm AWD:",theResultList[[3]],"m.",round(theResultList[[3]]/currentDroneRange,nPrecDigit), "du.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm chargings:",theResultList[[4]],"\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm opt path:",theResultList[[5]],"\n")
    
    
    
    shortestPathCostCW <- theResultList[[1]]
    shortestPathLenCW <- theResultList[[2]]
    AWDCW <- theResultList[[3]]
    nChargingCW <- theResultList[[4]]
    shortestPathCW <- theResultList[[5]]
    shortestRescuePathListCW <- theResultList[[6]]
    shortestRescuePathListLenCW <- theResultList[[7]]
    
    #get the vector showing the order of rescue (permutation)
    rescueOrdCW <- boatPermCW + nBSandDepot
    
    printOrdCW <- sprintf("%s%d","B",(rescueOrdCW - (nDT + nBS)))
    
    
    
    infoTxt <- paste0(infoTxt,"ClockWise order: ",paste0(printOrdCW,collapse = "->"),"\n",
                      "Drone range: ", currentDroneRange,"m\n",
                      "The cost: ",round(shortestPathCostCW,nPrecDigit),"m, hops: ", shortestPathLenCW,"\n",
                      "AWD: ",round(AWDCW,nPrecDigit),"m\n",
                      "Number of full chargings: ",nChargingCW,"\n",
                      "The Path: ",shortestPathCW,"\n",
                      "---------------------------------------------\n")
    
    
    
    
    
    cat("\n*********************************END CLOCKWISE*****************************************\n")
    
    ###################################################### END CLOCKWISE ############################################################
    
    
    
    
    ###################################################### BEGIN ANTI-CLOCKWISE ###########################################################
    
    #DEBUG
    cat("\n*********************************BEGIN ANTI-CLOCKWISE*****************************************\n")
    
    
    ################################################
    #We need an array or a vector to store the rescuePaths for each Boat!!!
    #Later we will check the intersections and stuff
    
    rescuePathList <- list()
    ################################################
    
    #########################################################################################
    #BEGIN: For many boat one BS
    #########################################################################################
    #First you need to update the adjacency mtx
    #For this you need to find chains of boat that a drone can traverse without refueling
    #Think of the algo to do this in an efficient way
    #Memory should not be a problem so find the fastest way
    #Create matrix for boat-to-boat distances
    #How about thinking of the completely connected graph of boats only
    #The goal:
    #Find the shortest path that can contain the most number of boats
    #The path length should be limited to the drone range
    #At the end of the path and at the beginning you should have depots(fueling stations)
    #So basically you find a path between two depots, "drone-range" apart
    #and in between you squeeze as many boat as you can
    #Obviously you need to consider starting with two boats that they have distance
    #less than the drone range and you can connect them to depots
    #
    
    
    #DEBUG cat("NBoat-1BS\n")
    #Clear message area 
    #my_text <-""
    #output$msg1<- renderText({  })
    
    #Here is a brute force way to find the TSP tour involving boats
    #Try all the permutations of the boats by finding sp from one to another
    #Then pick the min path!!!
    #cat("Boats:",reac()$nBoat,"\n")
    boatVids <- c((nDT + nBS + 1) : (nDT + nBS + nBT) ) # vector for vertex ids
    cat("ACW - Boat vertex ids:",boatVids,"\n")
    boatPermACW <- DistToBSSortedPermACW
    cat("ACW - Boat perm vertex ids:",boatPermACW,"\n")
    
    
    
    ###################################################### STAT ANTI-CLOCKWISE ###########################################################
    theResultList <- my_get_metrics_permDEBUG(gTempACW, boatPermACW, currentDroneRange)
    # theResult[[1]] <- totPathCost 
    # theResult[[2]] <- totPathLen
    # theResult[[3]] <- AWD 
    # theResult[[4]] <- nCharging
    # theResult[[5]] <- path2print
    
    #DEBUG
    cat("ACW my_get_metrics_permDEBUG -- Drone Range is:", currentDroneRange,"m.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm cost is:",theResultList[[1]],"m.",round(theResultList[[1]]/currentDroneRange,nPrecDigit),"du.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm hops:",theResultList[[2]],"\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm AWD:",theResultList[[3]],"m.",round(theResultList[[3]]/currentDroneRange,nPrecDigit), "du.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm chargings:",theResultList[[4]],"\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm opt path:",theResultList[[5]],"\n")
    
    
    shortestPathCostACW <- theResultList[[1]]
    shortestPathLenACW <- theResultList[[2]]
    AWDACW <- theResultList[[3]]
    nChargingACW <- theResultList[[4]]
    shortestPathACW <- theResultList[[5]]
    shortestRescuePathListACW <- theResultList[[6]]
    shortestRescuePathListLenACW <- theResultList[[7]]
    
    #get the vector showing the order of rescue (permutation)
    rescueOrdACW <- boatPermACW + nBSandDepot
    
    printOrdACW <- sprintf("%s%d","B",(rescueOrdACW - (nDT + nBS)))
    
    infoTxt <- paste0(infoTxt,"AntiClockWise order: ",paste0(printOrdACW,collapse = "->"),"\n",
                      "Drone range: ", currentDroneRange,"m\n",
                      "The cost: ",round(shortestPathCostACW,nPrecDigit),"m, hops: ", shortestPathLenACW,"\n",
                      "AWD: ",round(AWDACW,nPrecDigit),"m\n",
                      "Number of full chargings: ",nChargingACW,"\n",
                      "The Path: ",shortestPathACW,"\n",
                      "---------------------------------------------\n")
    
    
    
    
    cat("\n*********************************END ANTI-CLOCKWISE*****************************************\n")
    
    
    ###################################################### END ANTI-CLOCKWISE ###########################################################
    
    elapsed <- toc()
    
    elapsedSec <- elapsed$toc - elapsed$tic
    cat(algo, elapsedSec, "seconds\n")
    
    ###################################################### STAT COMBINED ###########################################################
    
    #Here compare CW and ACW direction data and find the shortest path
    if (round(shortestPathCostACW) < round(shortestPathCostCW)) {
      #Anti ClockWise is better 
      cat(algo,": ACW has shorter dist:",round(shortestPathCostACW,nPrecDigit),"m. Selecting ACW\n")
      infoTxt <- paste0(infoTxt,algo,": ACW has shorter dist:",round(shortestPathCostACW,nPrecDigit),"m. Selecting ACW\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat(algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat(algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
      }else{
        #They are equal
        cat(algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
      }
      
      
      #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
      
      boatOrder <- paste0(printOrdACW,collapse = "-")
      theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostACW, 3), round(AWDACW, 3), 
                                nChargingACW, nBSandDepot, round(elapsedSec, 3),
                                boatPosFileName, boatOrder, shortestPathACW, sep=", ")
      
      # #Here color the path
      # for (cnt in (1:shortestRescuePathListLenACW)) {
      #   E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$color <- rescuePathCol
      #   E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$width <- 2
      # }
      # myGraph$value <- gTempACW
    }else if(round(shortestPathCostACW) > round(shortestPathCostCW)) {
      #ClockWise is better
      cat(algo,": CW has shorter dist:",round(shortestPathCostCW,nPrecDigit),"m. Selecting CW\n")
      infoTxt <- paste0(infoTxt,algo,": CW has shorter dist:",round(shortestPathCostCW,nPrecDigit),"m. Selecting CW\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat(algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat(algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
      }else{
        #They are equal
        cat(algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
      }
      
      
      
      #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
      
      boatOrder <- paste0(printOrdCW,collapse = "-")
      theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostCW, 3), round(AWDCW, 3), 
                                nChargingCW, nBSandDepot, round(elapsedSec, 3),
                                boatPosFileName, boatOrder, shortestPathCW, sep=", ")
      
      # #Here color the path
      # for (cnt in (1:shortestRescuePathListLenCW)) {
      #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
      #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
      # }
      # myGraph$value <- gTempCW
    }else{
      #They are equal so choose the one with better AWD
      cat(algo,": Both ACW and CW has same dist:",round(shortestPathCostCW,nPrecDigit),"m\n")
      infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same dist:",round(shortestPathCostCW,nPrecDigit),"m\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat(algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m. Selecting ACW\n")
        infoTxt <- paste0(infoTxt,algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m. Selecting ACW\n")
        
        
        #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
        
        boatOrder <- paste0(printOrdACW,collapse = "-")
        theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostACW, 3), round(AWDACW, 3), 
                                  nChargingACW, nBSandDepot, round(elapsedSec, 3),
                                  boatPosFileName, boatOrder, shortestPathACW, sep=", ")
        
        # #Here color the path
        # for (cnt in (1:shortestRescuePathListLenACW)) {
        #   E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$color <- rescuePathCol
        #   E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$width <- 2
        # }
        # myGraph$value <- gTempACW
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat(algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m. Selecting CW\n")
        infoTxt <- paste0(infoTxt,algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m. Selecting CW\n")
        
        
        #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
        
        boatOrder <- paste0(printOrdCW,collapse = "-")
        theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostCW, 3), round(AWDCW, 3), 
                                  nChargingCW, nBSandDepot, round(elapsedSec, 3),
                                  boatPosFileName, boatOrder, shortestPathCW, sep=", ")
        
        # #Here color the path
        # for (cnt in (1:shortestRescuePathListLenCW)) {
        #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
        #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
        # }
        # myGraph$value <- gTempCW
      }else{
        #They are equal
        cat(algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m choosing CW\n")
        infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m choosing CW\n")
        
        
        #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
        
        boatOrder <- paste0(printOrdCW,collapse = "-")
        theRowForResults <- paste(activeGridType,algo,nBT, round(shortestPathCostCW, 3), round(AWDCW, 3), 
                                  nChargingCW, nBSandDepot, round(elapsedSec, 3),
                                  boatPosFileName, boatOrder, shortestPathCW, sep=", ")
        # #Here color the path
        # for (cnt in (1:shortestRescuePathListLenCW)) {
        #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
        #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
        # }
        # myGraph$value <- gTempCW
      }
      
      
      
      
      # #Here color the path
      # for (cnt in (1:shortestRescuePathListLenCW)) {
      #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
      #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
      # }
      # myGraph$value <- gTempCW
    }
    
    infoTxt <- paste0(infoTxt,"---------------------------------------------\n")
    
    
    
    
    
    #infoTxt <- paste0(infoTxt,"---------------------------------------------\n")
    
    ###################################################### STAT COMBINED ###########################################################
    # output$info<- renderUI(
    #   HTML(
    #     paste(
    #       c("<pre>", capture.output(cat(infoTxt)), "</pre>"),
    #       collapse = "<br>"
    #     )
    #   )
    # )
    
    cat(infoTxt,"\n")
    
    
    #########################################################################################
    #END: For many boat one BS
    #########################################################################################
    
    # isolate({
    #   new_zoom <- 9
    #   if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
    # })
    
    # cat("Rescue Lines are drawn.\n")
    
    
    
    
    
    cat(theRowForResults ,"\n",file=resultFile,append=T)
    cat(algo, elapsedSec, "seconds\n")
    cat("\n*********************************END",algo,"*****************************************\n")    
  }
  ########################################################################################################
  
  
  #Calling wrappers for button press
  ######################################################################################################## 
  observeEvent(input$clookRedGray,{
    cat("\n Calling  clook4UI(rg)\n")
    
    clookforUI("rg")
  })
  
  observeEvent(input$clookGray,{
    cat("\n Calling  clook4UI(g)\n")
    
    clookforUI("g")
  })
  
  observeEvent(input$clookRedGrayYellow,{
    cat("\n Calling  clook4UI(rgy)\n")
    
    clookforUI("rgy")
  })
  
  
  
  
  ######################################################################################################## 
  
  
  
  #########################################################################################################
  #########################################################################################################
  #OK uses gTempACW and gTempCW
  #OK AWD
  #OK Number of chargings
  #OK Red edge to BS from the last Boat without going to gray edge
  #########################################################################################################
  #########################################################################################################
  clookforUI <- function (edgeType){ 
    
    
    
    
    #Assuming that we have the graph save local copy to work on it
    #Both for CW and ACW
    if (edgeType == "g"){
      algo <- "clookGray"
      gTempCW <- gGray
      gTempACW <- gGray
      updateRadioButtons(
        session = session, 
        inputId = "edgeFilter",
        selected = "g"
      )
    }else if (edgeType == "rg"){
      algo <- "clookRedGray"
      gTempCW <- gRedGray
      gTempACW <- gRedGray
      updateRadioButtons(
        session = session, 
        inputId = "edgeFilter",
        selected = "rg"
      )
    }else if (edgeType == "rgy"){
      algo <- "clookRedGrayYellow"
      gTempCW <- gRedGrayYellow
      gTempACW <- gRedGrayYellow
      updateRadioButtons(
        session = session, 
        inputId = "edgeFilter",
        selected = "rgy"
      )
    }
    
    cat("\n*********************************BEGIN",algo,"*****************************************\n")
    
    tic(algo)
    
    
    
    #TODO: Red-gray paths are not checked!!!
    #Between two boats we need to check if there is a red-gray edge
    
    #Boats should be added to mtx when the user clicks
    #Here just find the sp and plot it!!!
    
    cat(algo,":", nBT ,"boats with",nBSandDepot,"bs+depots.\n")
    
    currentDroneRange <- reac()$drone_range
    
   
    
    ################################################
    #CC Analysis of the graph. Good for multiple BSs
    #Does not matter which gTemp
    #Initially they are same!!!!
    ################################################
    #Here check if depots are connected or isolated label them!!!
    cl <- components(gTempCW)
    #DEBUG
    cat("There are", cl$no, "CCs in the graph\n");
    cclist <- lapply(seq_along(cl$csize)[cl$csize > 0], function(x) V(gTempCW)$label[cl$membership %in% x])
    
    #Lets find the group BS is in!!!
    BSccId <- which("BS1" %in% V(gTempCW)$label[cl$membership])
    #May be BSccId <- cl$membership[1]
    
    ################################################
    #DEBUG
    # cat("CClist:", unlist(cclist), "\n")
    cat("CCs are:\n")
    cat("--------\n")
    for (i in (1:length(cclist))) {
      cat("CC",i,"--",cclist[[i]], "\n")
    }
    cat("\n")
    ################################################
    
    
    #Convert BTMtx to dataframe and sort the points clockwise
    BTDF <- as.data.frame(BTMtx)
    colnames(BTDF)<- c("lon","lat")
    #Add column for original boat numbers
    BTDF$BNo <- seq.int(nrow(BTDF))
    BTDF$DistToBS <- apply(BTMtx, 1, function(x) distGeo(x, BSMtx[1,]))
    sortedBTMtx <- my_sort_points(BTDF, y="lat", x="lon", bsx=BSMtx[1,1], bsy=BSMtx[1,2])
    
    BTGroups <- split(sortedBTMtx, cut(sortedBTMtx$angle_degrees, breaks = sectors, labels = seq(1:(nSectors-1))), drop = TRUE)
    nBTGroups <- length(BTGroups)
    
    #DEBUG
    cat("---------------------------------------------------------------------------------------------\n")
    cat("Sorted boats according to angle:\n")
    cat("---------------------------------------------------------------------------------------------\n")
    print(sortedBTMtx)
    cat("---------------------------------------------------------------------------------------------\n")
    cat(nBTGroups, "Groups according to angle sectors and inside each group sorted according to Dist to BS:\n")
    cat("Sector groups:",sectors,"\n")
    cat("---------------------------------------------------------------------------------------------\n")
    print(BTGroups)
    cat("---------------------------------------------------------------------------------------------\n")
    
    # 
    # 
    # 
    # 
    # nchunks <- ceiling( 
    #   (
    #     ceiling(sign(max(sortedBTMtx$angle_degrees)) * max(sortedBTMtx$angle_degrees)) - 
    #     (sign(min(sortedBTMtx$angle_degrees)) * ceiling(sign(min(sortedBTMtx$angle_degrees)) * min(sortedBTMtx$angle_degrees)))
    #   )
    #   / BTGroupSectorAngle) + 1
    
    
    
    if (nBTGroups > 1) {
      
      #We have many sector groups!!!!
      
      #DEBUG
      cat(algo,": ",nBTGroups,"BT sector groups.\n")
      
      DistToBSSortedPermCW <- NULL
      DistToBSSortedPermACW <- NULL
      
      DistToBSSortedPermCWlist <- list()
      DistToBSSortedPermACWlist <- list()
      
      #HERE MODIFY THE ALGO - add the "dynamic direction" twist!!!
      #Conservative up-down scanning:
      #It seems when scan changes sector you need to find the closest boat from the next sector to go
      # You need to consider the top and bottom boats in the sector
      #So there will be no fixed direction
      #Basically when you go to the next sector you jump to the closest boat or GrayDepot and then scan the others
      # So if the closest boat (or depot!!!) is the top(furthest) one then direction will be down(towards center or towards the BS)
      #if the closest boat is the bottom(closest to BS) one then the direction will be up (away from the BS)
      
      
      
      #HERE THERE IS A BUG IN DIRS
      #TEST WITH TWO GROUPS
      
      ####################################################################################
      #Loop for CW
      #The first sector should be away from BS and the last one should be towards BS!!!
      ####################################################################################
      ScanDir <- 1 #up Away from BS
      
      for (btx in (nBTGroups:1)) {
        
        groupDF <- as.data.frame(BTGroups[btx])
        colnames(groupDF) <- colnames(sortedBTMtx)
        
        if (ScanDir == 1) {
          #Away from BS
          sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = FALSE),] 
          ScanDir <- 0 #Change to Down to BS
        }else{
          #Towards BS
          sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
          ScanDir <- 1 #Change to Up again away from BS
        }
        
        #For CW the last group, "1", should be towards BS
        if (btx == 1) {
          sortedGroupDFX <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
          DistToBSSortedPermCW <- c(DistToBSSortedPermCW, sortedGroupDFX$BNo)
          DistToBSSortedPermCWlist[[nBTGroups - btx + 1]] <- sortedGroupDFX
        }else{
          DistToBSSortedPermCW <- c(DistToBSSortedPermCW, sortedGroupDF$BNo)
          DistToBSSortedPermCWlist[[nBTGroups - btx + 1]] <- sortedGroupDF
        }
      }#end for loop on nBTGroups
      
      
      
      
      
      ####################################################################################
      #Loop for ACW
      #The first sector should be away from BS and the last one should be towards BS!!!
      ####################################################################################
      ScanDir <- 1 #up Away from BS
      
      for (btx in (1:nBTGroups)) {
        
        groupDF <- as.data.frame(BTGroups[btx])
        colnames(groupDF) <- colnames(sortedBTMtx)
        
        if (ScanDir == 1) {
          #Away from BS
          sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = FALSE),] 
          ScanDir <- 0 #Change to Down to BS
        }else{
          #Towards BS
          sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
          ScanDir <- 1 #Change to Up again away from BS
        }
        
        #For ACW the last group, "nBTGroups", should be towards BS
        if (btx == nBTGroups) {
          sortedGroupDFX <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
          DistToBSSortedPermACW <- c(DistToBSSortedPermACW, sortedGroupDFX$BNo)
          DistToBSSortedPermACWlist[[btx]] <- sortedGroupDFX
        }else{
          DistToBSSortedPermACW <- c(DistToBSSortedPermACW, sortedGroupDF$BNo)
          DistToBSSortedPermACWlist[[btx]] <- sortedGroupDF
        }
      }#end for loop on nBTGroups
      
    }else{
      #When we have a single sector of boats!!!!
      #DEBUG
     
      cat(algo,": ",nBTGroups,"BT sector groups.\n")
      
      DistToBSSortedPermCW <- NULL
      DistToBSSortedPermACW <- NULL
      
      ScanDir <- 1 #up
      groupDF <- as.data.frame(BTGroups)
      colnames(groupDF) <- colnames(sortedBTMtx)
      if (ScanDir == 1) {
        sortedGroupDF <- groupDF[order(groupDF$DistToBS),] 
        ScanDir <- 0 #Down
      }else{
        sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
        ScanDir <- 1 #Up again
      }
      DistToBSSortedPermCW <- c(DistToBSSortedPermCW, sortedGroupDF$BNo)
      DistToBSSortedPermACW <- c(sortedGroupDF$BNo, DistToBSSortedPermACW)
    }
    
    nperm <- 1
    
    #DEBUG
    cat("BS:",nBS," Depots:", nDT, " Boats:", nBT, "Trying Permutation:", sortedBTMtx$BNo,"CW sorted perm:",DistToBSSortedPermCW ,"\n")
    cat("BS:",nBS," Depots:", nDT, " Boats:", nBT, "Trying Permutation:", sortedBTMtx$BNo,"ACW sorted perm:",DistToBSSortedPermACW ,"\n")
    
    
    infoTxt <- paste0("---------------------------------------------\n",
                      algo,":\n",
                      "---------------------------------------------\n",
                      "BS: ",1," Depots: ", nDT, " Boats: ", nBT, " Permutations: ", factorial(nBT),"\n",
                      "---------------------------------------------\n")
    
    
    
    
    
    ###################################################### BEGIN CLOCKWISE ############################################################
    
    #DEBUG
    cat("\n*********************************BEGIN CLOCKWISE*****************************************\n")
   
    ################################################
    #We need an array or a vector to store the rescuePaths for each Boat!!!
    #Later we will check the intersections and stuff
    
    rescuePathList <- list()
    ################################################
    
    #########################################################################################
    #BEGIN: For many boat one BS
    #########################################################################################
    #First you need to update the adjacency mtx
    #For this you need to find chains of boat that a drone can traverse without refueling
    #Think of the algo to do this in an efficient way
    #Memory should not be a problem so find the fastest way
    #Create matrix for boat-to-boat distances
    #How about thinking of the completely connected graph of boats only
    #The goal:
    #Find the shortest path that can contain the most number of boats
    #The path length should be limited to the drone range
    #At the end of the path and at the beginning you should have depots(fueling stations)
    #So basically you find a path between two depots, "drone-range" apart
    #and in between you squeeze as many boat as you can
    #Obviously you need to consider starting with two boats that they have distance
    #less than the drone range and you can connect them to depots
    #
    
    
    #DEBUG cat("NBoat-1BS\n")
    #Clear message area 
    my_text <-""
    output$msg1<- renderText({  })
    
    #Here is a brute force way to find the TSP tour involving boats
    #Try all the permutations of the boats by finding sp from one to another
    #Then pick the min path!!!
    #cat("Boats:",reac()$nBoat,"\n")
    boatVids <- c((nDT + nBS + 1) : (nDT + nBS + nBT) ) # vector for vertex ids
    cat("CW - Boat vertex ids:",boatVids,"\n")
    boatPermCW <- DistToBSSortedPermCW
    cat("CW - Boat perm vertex ids:",boatPermCW,"\n")
    
    
    #DEBUG
    #costOfPerm <- my_get_cost_permDEBUG(gTempCW, boatPermCW, distWeight, plWeight, awgWaitingWeight, numChargingWeight, currentDroneRange)
    #cat("my_get_cost_permDEBUG -- Weighted boat perm cost is:",costOfPerm,"\n")
    
    
    
    ###################################################### STAT CLOCKWISE ###########################################################
    theResultList <- my_get_metrics_permDEBUG(gTempCW, boatPermCW, currentDroneRange)
    # theResult[[1]] <- totPathCost 
    # theResult[[2]] <- totPathLen
    # theResult[[3]] <- AWD 
    # theResult[[4]] <- nCharging
    # theResult[[5]] <- path2print
    
    #DEBUG
    cat("CW my_get_metrics_permDEBUG -- Drone Range is:", currentDroneRange,"m.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm cost is:",theResultList[[1]],"m.",round(theResultList[[1]]/currentDroneRange,nPrecDigit),"du.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm hops:",theResultList[[2]],"\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm AWD:",theResultList[[3]],"m.",round(theResultList[[3]]/currentDroneRange,nPrecDigit), "du.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm chargings:",theResultList[[4]],"\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm opt path:",theResultList[[5]],"\n")
    
    
    
    shortestPathCostCW <- theResultList[[1]]
    shortestPathLenCW <- theResultList[[2]]
    AWDCW <- theResultList[[3]]
    nChargingCW <- theResultList[[4]]
    shortestPathCW <- theResultList[[5]]
    shortestRescuePathListCW <- theResultList[[6]]
    shortestRescuePathListLenCW <- theResultList[[7]]
    
    #get the vector showing the order of rescue (permutation)
    rescueOrdCW <- boatPermCW + nBSandDepot
    
    printOrdCW <- sprintf("%s%d","B",(rescueOrdCW - (nDT + nBS)))
    
    
    
    infoTxt <- paste0(infoTxt,"ClockWise order: ",paste0(printOrdCW,collapse = "->"),"\n",
                      "Drone range: ", currentDroneRange,"m\n",
                      "The cost: ",round(shortestPathCostCW,nPrecDigit),"m, hops: ", shortestPathLenCW,"\n",
                      "AWD: ",round(AWDCW,nPrecDigit),"m\n",
                      "Number of full chargings: ",nChargingCW,"\n",
                      "The Path: ",shortestPathCW,"\n",
                      "---------------------------------------------\n")
    
    
    
    
    
    cat("\n*********************************END CLOCKWISE*****************************************\n")
    
    ###################################################### END CLOCKWISE ############################################################
    
    
    
    
    ###################################################### BEGIN ANTI-CLOCKWISE ###########################################################
    
    #DEBUG
    cat("\n*********************************BEGIN ANTI-CLOCKWISE*****************************************\n")
    
    ################################################
    #We need an array or a vector to store the rescuePaths for each Boat!!!
    #Later we will check the intersections and stuff
    
    rescuePathList <- list()
    ################################################
    
    #########################################################################################
    #BEGIN: For many boat one BS
    #########################################################################################
    #First you need to update the adjacency mtx
    #For this you need to find chains of boat that a drone can traverse without refueling
    #Think of the algo to do this in an efficient way
    #Memory should not be a problem so find the fastest way
    #Create matrix for boat-to-boat distances
    #How about thinking of the completely connected graph of boats only
    #The goal:
    #Find the shortest path that can contain the most number of boats
    #The path length should be limited to the drone range
    #At the end of the path and at the beginning you should have depots(fueling stations)
    #So basically you find a path between two depots, "drone-range" apart
    #and in between you squeeze as many boat as you can
    #Obviously you need to consider starting with two boats that they have distance
    #less than the drone range and you can connect them to depots
    #
    
    
    #DEBUG cat("NBoat-1BS\n")
    #Clear message area 
    #my_text <-""
    #output$msg1<- renderText({  })
    
    #Here is a brute force way to find the TSP tour involving boats
    #Try all the permutations of the boats by finding sp from one to another
    #Then pick the min path!!!
    #cat("Boats:",reac()$nBoat,"\n")
    boatVids <- c((nDT + nBS + 1) : (nDT + nBS + nBT) ) # vector for vertex ids
    cat("ACW - Boat vertex ids:",boatVids,"\n")
    boatPermACW <- DistToBSSortedPermACW
    cat("ACW - Boat perm vertex ids:",boatPermACW,"\n")
    
    
    
    ###################################################### STAT ANTI-CLOCKWISE ###########################################################
    theResultList <- my_get_metrics_permDEBUG(gTempACW, boatPermACW, currentDroneRange)
    # theResult[[1]] <- totPathCost 
    # theResult[[2]] <- totPathLen
    # theResult[[3]] <- AWD 
    # theResult[[4]] <- nCharging
    # theResult[[5]] <- path2print
    
    #DEBUG
    cat("ACW my_get_metrics_permDEBUG -- Drone Range is:", currentDroneRange,"m.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm cost is:",theResultList[[1]],"m.",round(theResultList[[1]]/currentDroneRange,nPrecDigit),"du.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm hops:",theResultList[[2]],"\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm AWD:",theResultList[[3]],"m.",round(theResultList[[3]]/currentDroneRange,nPrecDigit), "du.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm chargings:",theResultList[[4]],"\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm opt path:",theResultList[[5]],"\n")
    
    
    shortestPathCostACW <- theResultList[[1]]
    shortestPathLenACW <- theResultList[[2]]
    AWDACW <- theResultList[[3]]
    nChargingACW <- theResultList[[4]]
    shortestPathACW <- theResultList[[5]]
    shortestRescuePathListACW <- theResultList[[6]]
    shortestRescuePathListLenACW <- theResultList[[7]]
    
    #get the vector showing the order of rescue (permutation)
    rescueOrdACW <- boatPermACW + nBSandDepot
    
    printOrdACW <- sprintf("%s%d","B",(rescueOrdACW - (nDT + nBS)))
    
    infoTxt <- paste0(infoTxt,"AntiClockWise order: ",paste0(printOrdACW,collapse = "->"),"\n",
                      "Drone range: ", currentDroneRange,"m\n",
                      "The cost: ",round(shortestPathCostACW,nPrecDigit),"m, hops: ", shortestPathLenACW,"\n",
                      "AWD: ",round(AWDACW,nPrecDigit),"m\n",
                      "Number of full chargings: ",nChargingACW,"\n",
                      "The Path: ",shortestPathACW,"\n",
                      "---------------------------------------------\n")
    
    
    
    
    cat("\n*********************************END ANTI-CLOCKWISE*****************************************\n")
    
    
    ###################################################### END ANTI-CLOCKWISE ###########################################################
    
    
    
    
    elapsed <- toc()
    
    elapsedSec <- elapsed$toc - elapsed$tic
    cat(algo, elapsedSec, "seconds\n")
    
    ###################################################### STAT COMBINED ###########################################################
    
    #Here compare CW and ACW direction data and find the shortest path
    if (round(shortestPathCostACW) < round(shortestPathCostCW)) {
      #Anti ClockWise is better 
      cat(algo,": ACW has shorter dist:",round(shortestPathCostACW,nPrecDigit),"m. Selecting ACW\n")
      infoTxt <- paste0(infoTxt,algo,": ACW has shorter dist:",round(shortestPathCostACW,nPrecDigit),"m. Selecting ACW\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat(algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat(algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
      }else{
        #They are equal
        cat(algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
      }
      
      
      #theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
      
      boatOrder <- paste0(printOrdACW,collapse = "-")
    
      
      #Here color the path
      for (cnt in (1:shortestRescuePathListLenACW)) {
        E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$color <- rescuePathCol
        E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$width <- 2
      }
      myGraph$value <- gTempACW
      
    }else if(round(shortestPathCostACW) > round(shortestPathCostCW)) {
      #ClockWise is better
      cat(algo,": CW has shorter dist:",round(shortestPathCostCW,nPrecDigit),"m. Selecting CW\n")
      infoTxt <- paste0(infoTxt,algo,": CW has shorter dist:",round(shortestPathCostCW,nPrecDigit),"m. Selecting CW\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat(algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat(algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
      }else{
        #They are equal
        cat(algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
      }
      
      
      
     
      
      #Here color the path
      for (cnt in (1:shortestRescuePathListLenCW)) {
        E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
        E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
      }
      myGraph$value <- gTempCW
      
    }else{
      #They are equal so choose the one with better AWD
      cat(algo,": Both ACW and CW has same dist:",round(shortestPathCostCW,nPrecDigit),"m\n")
      infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same dist:",round(shortestPathCostCW,nPrecDigit),"m\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat(algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m. Selecting ACW\n")
        infoTxt <- paste0(infoTxt,algo,": ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m. Selecting ACW\n")
        
        
       
        #Here color the path
        for (cnt in (1:shortestRescuePathListLenACW)) {
          E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$color <- rescuePathCol
          E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$width <- 2
        }
        myGraph$value <- gTempACW
        
        
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat(algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m. Selecting CW\n")
        infoTxt <- paste0(infoTxt,algo,": CW has shorter AWD:",round(AWDCW,nPrecDigit),"m. Selecting CW\n")
        
        
        
        
        #Here color the path
        for (cnt in (1:shortestRescuePathListLenCW)) {
          E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
          E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
        }
        myGraph$value <- gTempCW
        
        
      }else{
        #They are equal
        cat(algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m choosing CW\n")
        infoTxt <- paste0(infoTxt,algo,": Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m choosing CW\n")
        
      
        #Here color the path
        for (cnt in (1:shortestRescuePathListLenCW)) {
          E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
          E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
        }
        myGraph$value <- gTempCW
        
      }
      
      
      
      
      # #Here color the path
      # for (cnt in (1:shortestRescuePathListLenCW)) {
      #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
      #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
      # }
      # myGraph$value <- gTempCW
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    infoTxt <- paste0(infoTxt,"---------------------------------------------\n")
    
    
    
    
    
  
    
    ###################################################### STAT COMBINED ###########################################################
    output$info<- renderUI(
      HTML(
        paste(
          c("<pre>", capture.output(cat(infoTxt)), "</pre>"),
          collapse = "<br>"
        )
      )
    )
    
    cat(infoTxt,"\n")
    
    
    #########################################################################################
    #END: For many boat one BS
    #########################################################################################
    
    isolate({
      new_zoom <- 9
      if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
    })
    
    cat("Rescue Lines are drawn.\n")
    
    
    cat(algo, elapsedSec, "seconds\n")
    cat("\n*********************************END",algo,"*****************************************\n")    
    
     
  }
  ########################################################################################################
  
  
  
  
  #########################################################################################################
  #########################################################################################################
  #OK uses gTempACW and gTempCW
  #OK AWD
  #OK Number of chargings
  #OK Red edge to BS from the last Boat without going to gray edge
  #########################################################################################################
  #########################################################################################################
  observeEvent(input$clookGrayx,{ 
    
    cat("\n*********************************BEGIN clookGray*****************************************\n")
    
    tic("clookG")
    #TODO: Red-gray paths are not checked!!!
    #Between two boats we need to check if there is a red-gray edge
    
    #Boats should be added to mtx when the user clicks
    #Here just find the sp and plot it!!!
    
    cat("clook with Gray:", nBT ,"boats with",nBSandDepot,"bs+depots.\n")
    
    currentDroneRange <- reac()$drone_range
    
    #Assuming that we have the graph save local copy to work on it
    #Both for CW and ACW
    gTempCW <- gGray
    gTempACW <- gGray
    
    updateRadioButtons(
      session = session, 
      inputId = "edgeFilter",
      selected = "g"
    )
    
    
    ################################################
    #CC Analysis of the graph. Good for multiple BSs
    #Does not matter which gTemp
    #Initially they are same!!!!
    ################################################
    #Here check if depots are connected or isolated label them!!!
    cl <- components(gTempCW)
    #DEBUG
    cat("There are", cl$no, "CCs in the graph\n");
    cclist <- lapply(seq_along(cl$csize)[cl$csize > 0], function(x) V(gTempCW)$label[cl$membership %in% x])
    
    #Lets find the group BS is in!!!
    BSccId <- which("BS1" %in% V(gTempCW)$label[cl$membership])
    #May be BSccId <- cl$membership[1]
    
    ################################################
    #DEBUG
    # cat("CClist:", unlist(cclist), "\n")
    cat("CCs are:\n")
    cat("--------\n")
    for (i in (1:length(cclist))) {
      cat("CC",i,"--",cclist[[i]], "\n")
    }
    cat("\n")
    ################################################
    
    
    #Convert BTMtx to dataframe and sort the points clockwise
    BTDF <- as.data.frame(BTMtx)
    colnames(BTDF)<- c("lon","lat")
    #Add column for original boat numbers
    BTDF$BNo <- seq.int(nrow(BTDF))
    BTDF$DistToBS <- apply(BTMtx, 1, function(x) distGeo(x, BSMtx[1,]))
    sortedBTMtx <- my_sort_points(BTDF, y="lat", x="lon", bsx=BSMtx[1,1], bsy=BSMtx[1,2])
    
    BTGroups <- split(sortedBTMtx, cut(sortedBTMtx$angle_degrees, breaks = sectors, labels = seq(1:(nSectors-1))), drop = TRUE)
    nBTGroups <- length(BTGroups)
    
    #DEBUG
    cat("---------------------------------------------------------------------------------------------\n")
    cat("Sorted boats according to angle:\n")
    cat("---------------------------------------------------------------------------------------------\n")
    print(sortedBTMtx)
    cat("---------------------------------------------------------------------------------------------\n")
    cat(nBTGroups, "Groups according to angle sectors and inside each group sorted according to Dist to BS:\n")
    cat("Sector groups:",sectors,"\n")
    cat("---------------------------------------------------------------------------------------------\n")
    print(BTGroups)
    cat("---------------------------------------------------------------------------------------------\n")
    
    # 
    # 
    # 
    # 
    # nchunks <- ceiling( 
    #   (
    #     ceiling(sign(max(sortedBTMtx$angle_degrees)) * max(sortedBTMtx$angle_degrees)) - 
    #     (sign(min(sortedBTMtx$angle_degrees)) * ceiling(sign(min(sortedBTMtx$angle_degrees)) * min(sortedBTMtx$angle_degrees)))
    #   )
    #   / BTGroupSectorAngle) + 1
    
    
    
    if (nBTGroups > 1) {
      
      #We have many sector groups!!!!
      
      #DEBUG
      cat("clookGray: ",nBTGroups,"BT sector groups.\n")
      
      DistToBSSortedPermCW <- NULL
      DistToBSSortedPermACW <- NULL
      
      DistToBSSortedPermCWlist <- list()
      DistToBSSortedPermACWlist <- list()
      
      #HERE MODIFY THE ALGO - add the "dynamic direction" twist!!!
      #Conservative up-down scanning:
      #It seems when scan changes sector you need to find the closest boat from the next sector to go
      # You need to consider the top and bottom boats in the sector
      #So there will be no fixed direction
      #Basically when you go to the next sector you jump to the closest boat or GrayDepot and then scan the others
      # So if the closest boat (or depot!!!) is the top(furthest) one then direction will be down(towards center or towards the BS)
      #if the closest boat is the bottom(closest to BS) one then the direction will be up (away from the BS)
      
      
      
      #HERE THERE IS A BUG IN DIRS
      #TEST WITH TWO GROUPS
      
      ####################################################################################
      #Loop for CW
      #The first sector should be away from BS and the last one should be towards BS!!!
      ####################################################################################
      ScanDir <- 1 #up Away from BS
      
      for (btx in (nBTGroups:1)) {
        
        groupDF <- as.data.frame(BTGroups[btx])
        colnames(groupDF) <- colnames(sortedBTMtx)
        
        if (ScanDir == 1) {
          #Away from BS
          sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = FALSE),] 
          ScanDir <- 0 #Change to Down to BS
        }else{
          #Towards BS
          sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
          ScanDir <- 1 #Change to Up again away from BS
        }
        
        #For CW the last group, "1", should be towards BS
        if (btx == 1) {
          sortedGroupDFX <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
          DistToBSSortedPermCW <- c(DistToBSSortedPermCW, sortedGroupDFX$BNo)
          DistToBSSortedPermCWlist[[nBTGroups - btx + 1]] <- sortedGroupDFX
        }else{
          DistToBSSortedPermCW <- c(DistToBSSortedPermCW, sortedGroupDF$BNo)
          DistToBSSortedPermCWlist[[nBTGroups - btx + 1]] <- sortedGroupDF
        }
      }#end for loop on nBTGroups
      
      
      
      
      
      ####################################################################################
      #Loop for ACW
      #The first sector should be away from BS and the last one should be towards BS!!!
      ####################################################################################
      ScanDir <- 1 #up Away from BS
      
      for (btx in (1:nBTGroups)) {
        
        groupDF <- as.data.frame(BTGroups[btx])
        colnames(groupDF) <- colnames(sortedBTMtx)
        
        if (ScanDir == 1) {
          #Away from BS
          sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = FALSE),] 
          ScanDir <- 0 #Change to Down to BS
        }else{
          #Towards BS
          sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
          ScanDir <- 1 #Change to Up again away from BS
        }
        
        #For ACW the last group, "nBTGroups", should be towards BS
        if (btx == nBTGroups) {
          sortedGroupDFX <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
          DistToBSSortedPermACW <- c(DistToBSSortedPermACW, sortedGroupDFX$BNo)
          DistToBSSortedPermACWlist[[btx]] <- sortedGroupDFX
        }else{
          DistToBSSortedPermACW <- c(DistToBSSortedPermACW, sortedGroupDF$BNo)
          DistToBSSortedPermACWlist[[btx]] <- sortedGroupDF
        }
      }#end for loop on nBTGroups
      
    }else{
      #When we have a single sector of boats!!!!
      #DEBUG
      cat("clookGray: ",nBTGroups,"BT sector groups.\n")
      
      DistToBSSortedPermCW <- NULL
      DistToBSSortedPermACW <- NULL
      
      ScanDir <- 1 #up
      groupDF <- as.data.frame(BTGroups)
      colnames(groupDF) <- colnames(sortedBTMtx)
      if (ScanDir == 1) {
        sortedGroupDF <- groupDF[order(groupDF$DistToBS),] 
        ScanDir <- 0 #Down
      }else{
        sortedGroupDF <- groupDF[order(groupDF$DistToBS, decreasing = TRUE),] 
        ScanDir <- 1 #Up again
      }
      DistToBSSortedPermCW <- c(DistToBSSortedPermCW, sortedGroupDF$BNo)
      DistToBSSortedPermACW <- c(sortedGroupDF$BNo, DistToBSSortedPermACW)
    }
    
    nperm <- 1
    
    #DEBUG
    cat("BS:",nBS," Depots:", nDT, " Boats:", nBT, "Trying Permutation:", sortedBTMtx$BNo,"CW sorted perm:",DistToBSSortedPermCW ,"\n")
    cat("BS:",nBS," Depots:", nDT, " Boats:", nBT, "Trying Permutation:", sortedBTMtx$BNo,"ACW sorted perm:",DistToBSSortedPermACW ,"\n")
    
    
    infoTxt <- paste0("---------------------------------------------\n",
                      "clookGray:\n",
                      "---------------------------------------------\n",
                      "BS: ",1," Depots: ", nDT, " Boats: ", nBT, " Permutations: ", factorial(nBT),"\n",
                      "---------------------------------------------\n")
    
    
    
    
    
    ###################################################### BEGIN CLOCKWISE ############################################################
    
    #DEBUG
    cat("\n*********************************BEGIN CLOCKWISE*****************************************\n")
    cat("Perm Rescue for", nBT ,"boats\n")
    
    ################################################
    #We need an array or a vector to store the rescuePaths for each Boat!!!
    #Later we will check the intersections and stuff
    
    rescuePathList <- list()
    ################################################
    
    #########################################################################################
    #BEGIN: For many boat one BS
    #########################################################################################
    #First you need to update the adjacency mtx
    #For this you need to find chains of boat that a drone can traverse without refueling
    #Think of the algo to do this in an efficient way
    #Memory should not be a problem so find the fastest way
    #Create matrix for boat-to-boat distances
    #How about thinking of the completely connected graph of boats only
    #The goal:
    #Find the shortest path that can contain the most number of boats
    #The path length should be limited to the drone range
    #At the end of the path and at the beginning you should have depots(fueling stations)
    #So basically you find a path between two depots, "drone-range" apart
    #and in between you squeeze as many boat as you can
    #Obviously you need to consider starting with two boats that they have distance
    #less than the drone range and you can connect them to depots
    #
    
    
    #DEBUG cat("NBoat-1BS\n")
    #Clear message area 
    my_text <-""
    output$msg1<- renderText({  })
    
    #Here is a brute force way to find the TSP tour involving boats
    #Try all the permutations of the boats by finding sp from one to another
    #Then pick the min path!!!
    #cat("Boats:",reac()$nBoat,"\n")
    boatVids <- c((nDT + nBS + 1) : (nDT + nBS + nBT) ) # vector for vertex ids
    cat("CW - Boat vertex ids:",boatVids,"\n")
    boatPermCW <- DistToBSSortedPermCW
    cat("CW - Boat perm vertex ids:",boatPermCW,"\n")
    
    
    #DEBUG
    #costOfPerm <- my_get_cost_permDEBUG(gTempCW, boatPermCW, distWeight, plWeight, awgWaitingWeight, numChargingWeight, currentDroneRange)
    #cat("my_get_cost_permDEBUG -- Weighted boat perm cost is:",costOfPerm,"\n")
    
    
    
    ###################################################### STAT CLOCKWISE ###########################################################
    theResultList <- my_get_metrics_permDEBUG(gTempCW, boatPermCW, currentDroneRange)
    # theResult[[1]] <- totPathCost 
    # theResult[[2]] <- totPathLen
    # theResult[[3]] <- AWD 
    # theResult[[4]] <- nCharging
    # theResult[[5]] <- path2print
    
    #DEBUG
    cat("CW my_get_metrics_permDEBUG -- Drone Range is:", currentDroneRange,"m.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm cost is:",theResultList[[1]],"m.",round(theResultList[[1]]/currentDroneRange,nPrecDigit),"du.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm hops:",theResultList[[2]],"\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm AWD:",theResultList[[3]],"m.",round(theResultList[[3]]/currentDroneRange,nPrecDigit), "du.\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm chargings:",theResultList[[4]],"\n")
    cat("CW my_get_metrics_permDEBUG -- Boat perm opt path:",theResultList[[5]],"\n")
    
    
    
    shortestPathCostCW <- theResultList[[1]]
    shortestPathLenCW <- theResultList[[2]]
    AWDCW <- theResultList[[3]]
    nChargingCW <- theResultList[[4]]
    shortestPathCW <- theResultList[[5]]
    shortestRescuePathListCW <- theResultList[[6]]
    shortestRescuePathListLenCW <- theResultList[[7]]
    
    #get the vector showing the order of rescue (permutation)
    rescueOrdCW <- boatPermCW + nBSandDepot
    
    printOrdCW <- sprintf("%s%d","B",(rescueOrdCW - (nDT + nBS)))
    
    
    
    infoTxt <- paste0(infoTxt,"ClockWise order: ",paste0(printOrdCW,collapse = "->"),"\n",
                      "Drone range: ", currentDroneRange,"m\n",
                      "The cost: ",round(shortestPathCostCW,nPrecDigit),"m, hops: ", shortestPathLenCW,"\n",
                      "AWD: ",round(AWDCW,nPrecDigit),"m\n",
                      "Number of full chargings: ",nChargingCW,"\n",
                      "The Path: ",shortestPathCW,"\n",
                      "---------------------------------------------\n")
    
    
    
    
    
    cat("\n*********************************END CLOCKWISE*****************************************\n")
    
    ###################################################### END CLOCKWISE ############################################################
    
    
    
    
    ###################################################### BEGIN ANTI-CLOCKWISE ###########################################################
    
    #DEBUG
    cat("\n*********************************BEGIN ANTI-CLOCKWISE*****************************************\n")
    cat("Perm Rescue for", nBT ,"boats\n")
    
    ################################################
    #We need an array or a vector to store the rescuePaths for each Boat!!!
    #Later we will check the intersections and stuff
    
    rescuePathList <- list()
    ################################################
    
    #########################################################################################
    #BEGIN: For many boat one BS
    #########################################################################################
    #First you need to update the adjacency mtx
    #For this you need to find chains of boat that a drone can traverse without refueling
    #Think of the algo to do this in an efficient way
    #Memory should not be a problem so find the fastest way
    #Create matrix for boat-to-boat distances
    #How about thinking of the completely connected graph of boats only
    #The goal:
    #Find the shortest path that can contain the most number of boats
    #The path length should be limited to the drone range
    #At the end of the path and at the beginning you should have depots(fueling stations)
    #So basically you find a path between two depots, "drone-range" apart
    #and in between you squeeze as many boat as you can
    #Obviously you need to consider starting with two boats that they have distance
    #less than the drone range and you can connect them to depots
    #
    
    
    #DEBUG cat("NBoat-1BS\n")
    #Clear message area 
    #my_text <-""
    #output$msg1<- renderText({  })
    
    #Here is a brute force way to find the TSP tour involving boats
    #Try all the permutations of the boats by finding sp from one to another
    #Then pick the min path!!!
    #cat("Boats:",reac()$nBoat,"\n")
    boatVids <- c((nDT + nBS + 1) : (nDT + nBS + nBT) ) # vector for vertex ids
    cat("ACW - Boat vertex ids:",boatVids,"\n")
    boatPermACW <- DistToBSSortedPermACW
    cat("ACW - Boat perm vertex ids:",boatPermACW,"\n")
    
    
    
    ###################################################### STAT ANTI-CLOCKWISE ###########################################################
    theResultList <- my_get_metrics_permDEBUG(gTempACW, boatPermACW, currentDroneRange)
    # theResult[[1]] <- totPathCost 
    # theResult[[2]] <- totPathLen
    # theResult[[3]] <- AWD 
    # theResult[[4]] <- nCharging
    # theResult[[5]] <- path2print
    
    #DEBUG
    cat("ACW my_get_metrics_permDEBUG -- Drone Range is:", currentDroneRange,"m.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm cost is:",theResultList[[1]],"m.",round(theResultList[[1]]/currentDroneRange,nPrecDigit),"du.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm hops:",theResultList[[2]],"\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm AWD:",theResultList[[3]],"m.",round(theResultList[[3]]/currentDroneRange,nPrecDigit), "du.\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm chargings:",theResultList[[4]],"\n")
    cat("ACW my_get_metrics_permDEBUG -- Boat perm opt path:",theResultList[[5]],"\n")
    
    
    shortestPathCostACW <- theResultList[[1]]
    shortestPathLenACW <- theResultList[[2]]
    AWDACW <- theResultList[[3]]
    nChargingACW <- theResultList[[4]]
    shortestPathACW <- theResultList[[5]]
    shortestRescuePathListACW <- theResultList[[6]]
    shortestRescuePathListLenACW <- theResultList[[7]]
    
    #get the vector showing the order of rescue (permutation)
    rescueOrdACW <- boatPermACW + nBSandDepot
    
    printOrdACW <- sprintf("%s%d","B",(rescueOrdACW - (nDT + nBS)))
    
    infoTxt <- paste0(infoTxt,"AntiClockWise order: ",paste0(printOrdACW,collapse = "->"),"\n",
                      "Drone range: ", currentDroneRange,"m\n",
                      "The cost: ",round(shortestPathCostACW,nPrecDigit),"m, hops: ", shortestPathLenACW,"\n",
                      "AWD: ",round(AWDACW,nPrecDigit),"m\n",
                      "Number of full chargings: ",nChargingACW,"\n",
                      "The Path: ",shortestPathACW,"\n",
                      "---------------------------------------------\n")
    
    
    
    
    cat("\n*********************************END ANTI-CLOCKWISE*****************************************\n")
    
    
    ###################################################### END ANTI-CLOCKWISE ###########################################################
    
    
    
    ###################################################### STAT COMBINED ###########################################################
    
    #Here compare CW and ACW direction data and find the shortest path
    if (round(shortestPathCostACW) < round(shortestPathCostCW)) {
      #Anti ClockWise is better 
      cat("clookGray: ACW has shorter dist:",round(shortestPathCostACW,nPrecDigit),"m. Selecting ACW\n")
      infoTxt <- paste0(infoTxt,"clookGray: ACW has shorter dist:",round(shortestPathCostACW,nPrecDigit),"m. Selecting ACW\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat("clookGray: ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,"clookGray: ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat("clookGray: CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,"clookGray: CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
      }else{
        #They are equal
        cat("clookGray: Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,"clookGray: Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
      }
      
      #Here color the path
      for (cnt in (1:shortestRescuePathListLenACW)) {
        E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$color <- rescuePathCol
        E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$width <- 2
      }
      myGraph$value <- gTempACW
    }else if(round(shortestPathCostACW) > round(shortestPathCostCW)) {
      #ClockWise is better
      cat("clookGray: CW has shorter dist:",round(shortestPathCostCW,nPrecDigit),"m. Selecting CW\n")
      infoTxt <- paste0(infoTxt,"clookGray: CW has shorter dist:",round(shortestPathCostCW,nPrecDigit),"m. Selecting CW\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat("clookGray: ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,"clookGray: ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m\n")
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat("clookGray: CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,"clookGray: CW has shorter AWD:",round(AWDCW,nPrecDigit),"m\n")
      }else{
        #They are equal
        cat("clookGray: Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
        infoTxt <- paste0(infoTxt,"clookGray: Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m\n")
      }
      
      #Here color the path
      for (cnt in (1:shortestRescuePathListLenCW)) {
        E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
        E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
      }
      myGraph$value <- gTempCW
    }else{
      #They are equal so choose the one with better AWD
      cat("clookGray: Both ACW and CW has same dist:",round(shortestPathCostCW,nPrecDigit),"m\n")
      infoTxt <- paste0(infoTxt,"clookGray: Both ACW and CW has same dist:",round(shortestPathCostCW,nPrecDigit),"m\n")
      
      if (round(AWDACW) < round(AWDCW)) {
        #Anti ClockWise is better in waiting dist
        cat("clookGray: ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m. Selecting ACW\n")
        infoTxt <- paste0(infoTxt,"clookGray: ACW has shorter AWD:",round(AWDACW,nPrecDigit),"m. Selecting ACW\n")
        #Here color the path
        for (cnt in (1:shortestRescuePathListLenACW)) {
          E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$color <- rescuePathCol
          E(gTempACW, path=c(shortestRescuePathListACW[[cnt]]))$width <- 2
        }
        myGraph$value <- gTempACW
      }else if(round(AWDACW) > round(AWDCW)) {
        #ClockWise is better in waiting dist
        cat("clookGray: CW has shorter AWD:",round(AWDCW,nPrecDigit),"m. Selecting CW\n")
        infoTxt <- paste0(infoTxt,"clookGray: CW has shorter AWD:",round(AWDCW,nPrecDigit),"m. Selecting CW\n")
        #Here color the path
        for (cnt in (1:shortestRescuePathListLenCW)) {
          E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
          E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
        }
        myGraph$value <- gTempCW
      }else{
        #They are equal
        cat("clookGray: Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m choosing CW\n")
        infoTxt <- paste0(infoTxt,"clookGray: Both ACW and CW has same AWD:",round(AWDCW,nPrecDigit),"m choosing CW\n")
        #Here color the path
        for (cnt in (1:shortestRescuePathListLenCW)) {
          E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
          E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
        }
        myGraph$value <- gTempCW
      }
      
      
      
      
      # #Here color the path
      # for (cnt in (1:shortestRescuePathListLenCW)) {
      #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$color <- rescuePathCol
      #   E(gTempCW, path=c(shortestRescuePathListCW[[cnt]]))$width <- 2
      # }
      # myGraph$value <- gTempCW
    }
    
    infoTxt <- paste0(infoTxt,"---------------------------------------------\n")
    
    
    
    
    
    #infoTxt <- paste0(infoTxt,"---------------------------------------------\n")
    
    ###################################################### STAT COMBINED ###########################################################
    output$info<- renderUI(
      HTML(
        paste(
          c("<pre>", capture.output(cat(infoTxt)), "</pre>"),
          collapse = "<br>"
        )
      )
    )
    
    cat(infoTxt,"\n")
    
    
    #########################################################################################
    #END: For many boat one BS
    #########################################################################################
    
    isolate({
      new_zoom <- 9
      if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
    })
    
    cat("Rescue Lines are drawn.\n")
    
    toc()
    cat("\n*********************************END clookG*****************************************\n")    
  })
  ########################################################################################################
  
  
  
  #########################################################################################################
  #END CLOOK BASED
  #########################################################################################################  
  
  
  
  
  
  ########################################################################################################
  #END MY ALGO
  ########################################################################################################
  
  
  
  
  
  
  
  
  
  ########################################################################################################
  observeEvent(input$cov,{
    if (input$cov){
      leafletProxy("mymap") %>%
        addPolygons(data = depotPolygons,
                    color ="red",
                    weight = 0.5,
                    opacity = 3,
                    group = "depotPolys") %>%
        
        #Draw circles for the range of depots and check the aspect ratio
        addCircles(data = as.data.frame(DTMtx), 
                   lat = ~ V2, 
                   lng = ~ V1, 
                   weight = 1, 
                   radius = rescueDist,
                   #popup = as.character("Depot"),
                   #label = as.character(paste0("Depot: ")),
                   color = "blue", 
                   fillOpacity = 0.1,
                   group = "depotCircles")
      
      
      
      mapToSave$current <- mapToSave$base %>%
        addPolygons(data = depotPolygons,
                    color ="red",
                    weight = 0.5,
                    opacity = 3,
                    group = "depotPolys") %>%
        
        #Draw circles for the range of depots and check the aspect ratio
        addCircles(data = as.data.frame(DTMtx), 
                   lat = ~ V2, 
                   lng = ~ V1, 
                   weight = 1, 
                   radius = rescueDist,
                   #popup = as.character("Depot"),
                   #label = as.character(paste0("Depot: ")),
                   color = "blue", 
                   fillOpacity = 0.1,
                   group = "depotCircles")
      
      
    }else{
      leafletProxy("mymap") %>% clearGroup("depotPolys")  %>% clearGroup("depotCircles")
      
    }
  })
  
  
  ########################################################################################################
  
  
  
  ########################################################################################################
  observeEvent(input$closePolygon,{
    #clear the polygons
    #p4 <- VtxMtx2SpatPolyDF(a <- list())
    if (input$startPoly) {
      updatedValue = !input$startPoly
      updateSwitchInput(
        session = session,
        inputId = "startPoly",
        value = updatedValue,
        disabled = TRUE
      )
      
      #enable("DroneAlt")
      
      if ( !is.null(regionVtxMtx) && (length(regionVtxMtx[,1])>=3) ){
        #enable("DroneNumber")
        
        polyBEGIN <<- FALSE
        bsBEGIN <<- TRUE
        btBEGIN <<- FALSE
        
        regionVtxMtx <<- rbind(regionVtxMtx, regionVtxMtx[1,])
        
        regionPoly <<-  st_polygon(list(regionVtxMtx))
        regionPolyBBox <<- st_bbox(regionPoly)
        regionPolyArea <<- st_area(regionPoly)
        
        cat("Region area is",areaPolygon(regionPoly[[1]]),"meter^2\n")
        
        regionPolyDF <<- VtxMtx2SpatPolyDF(regionVtxMtx)
        isolate({
          new_zoom <- 9
          if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
          #cat("Clear Selection:", input$mymap_center$lng, input$mymap_center$lat, new_zoom, "\n")
          
          #Find the bbox and overlay the map
          ####################################################################################
          frameBBX <- st_bbox(regionPolyDF)
          
          mapH <- lonlat2m(frameBBX$xmin,frameBBX$ymin,frameBBX$xmin,frameBBX$ymax)
          mapW <- lonlat2m(frameBBX$xmin,frameBBX$ymin,frameBBX$xmax,frameBBX$ymin)
          
          framePoly <- Polygon(rbind(c(frameBBX$xmin,frameBBX$ymin),c(frameBBX$xmin,frameBBX$ymax),
                                     c(frameBBX$xmax,frameBBX$ymax), c(frameBBX$xmax,frameBBX$ymin),
                                     c(frameBBX$xmin,frameBBX$ymin)))
          framePolys <- Polygons(list(framePoly),1)
          regionBBoxPoly <<- SpatialPolygons(list(framePolys), proj4string=sp::CRS("+init=epsg:3857"))
          
          #length(regionVtxMtx[,1])-1, 
          output$myMsg <- renderText({paste0("Closing poly with ",length(regionPolyDF@polygons[[1]]@Polygons[[1]]@coords[,1])-1,
                                             " points. BBox width:",round(mapW,nPrecDigit),
                                             " - height:",round(mapH,nPrecDigit) ," meters\n") })
          ####################################################################################
          
          
          leafletProxy("mymap") %>% clearShapes()
          mapToSave$current <- mapToSave$base %>% clearShapes()
          
          leafletProxy("mymap") %>% 
            addPolygons(data=regionPolyDF, weight = 2, fillColor = "red",fillOpacity = 0.2, layerId ="myPolyLayer")%>% 
            addPolygons(data=regionBBoxPoly, weight = 2, fill=FALSE, layerId ="myBBoxLayer")
          
          
          
          mapToSave$current <- mapToSave$base %>%
            addPolygons(data=regionPolyDF, weight = 2, fillColor = "red",fillOpacity = 0.2, layerId ="myPolyLayer")%>% 
            addPolygons(data=regionBBoxPoly, weight = 2, fill=FALSE, layerId ="myBBoxLayer")
          #setView(lng = input$lng, lat = input$lat, zoom = new_zoom)
          
          updateSwitchInput(
            session = session,
            inputId = "startBS",
            disabled = FALSE
          )
        })
      }else{
        output$myMsg <- renderText({paste("Not enough polygon points: You need at least 3 points!!\n") })
        if (!is.null(regionVtxMtx)) {
          mapToSave$current <- mapToSave$base %>% clearShapes()
          leafletProxy("mymap") %>% clearShapes()
          regionVtxMtx <<- NULL
          
        }
      }
    }else {
      output$myMsg <- renderText({paste("Not in start poly mode!!!\n")})
    }
    
    
  })
  ########################################################################################################
  
  
  
  ########################################################################################################
  #create the map
  output$mymap <- renderLeaflet({
    
    #Find the bbox and overlay the map
    ####################################################################################
    frameBBX <- st_bbox(regionPolyDF)
    nPts <- length(regionPolyDF@polygons[[1]]@Polygons[[1]]@coords[,1]) - 1
    mapH <- lonlat2m(frameBBX$xmin,frameBBX$ymin,frameBBX$xmin,frameBBX$ymax)
    mapW <- lonlat2m(frameBBX$xmin,frameBBX$ymin,frameBBX$xmax,frameBBX$ymin)
    
    framePoly <- Polygon(rbind(c(frameBBX$xmin,frameBBX$ymin),c(frameBBX$xmin,frameBBX$ymax),
                               c(frameBBX$xmax,frameBBX$ymax), c(frameBBX$xmax,frameBBX$ymin),
                               c(frameBBX$xmin,frameBBX$ymin)))
    framePolys <- Polygons(list(framePoly),1)
    regionBBoxPoly <<- SpatialPolygons(list(framePolys), proj4string=sp::CRS("+init=epsg:3857"))
    
    
    output$myMsg <- renderText({paste0("Poly with ",nPts, 
                                       " points. BBox width:",round(mapW,nPrecDigit),
                                       " - height:",round(mapH,nPrecDigit) ," meters\n") })
    ####################################################################################
    
    
    mapToSave$current <- mapToSave$base <- leaflet("mymap", options = leafletOptions(crs = leafletCRS('L.CRS.EPSG3857'))) %>% 
      # mapToSave$current <- mapToSave$base <- leaflet("mymap")  %>%
      
      # setView(lng = 13.508 , lat = 43.628, zoom = 15)  %>% #setting the view over Camerino
      
      setView(lng = 13.508 , lat = 43.628, zoom = 15)  %>% #setting the view over Camerino
      
      #setView(lng = input$mymap_center$lng, lat = input$mymap_center$lat, zoom = input$mymap_zoom)  %>%
      #addTiles(options = providerTileOptions(minZoom = 1, maxZoom = 30)) %>% 
      addTiles()  %>%
      # addCircles(data = data, lat = ~ latitude, lng = ~ longitude, weight = 1, 
      #            radius = ~sqrt(mag)*25000, popup = ~as.character(mag), 
      #            label = ~as.character(paste0("Magnitude: ", sep = " ", mag)), 
      #            color = ~pal(mag), fillOpacity = 0.5) %>% 
      addPolygons(data=regionPolyDF, weight = 2, fillColor = "red",fillOpacity = 0.2, layerId ="myPolyLayer")%>%
      addPolygons(data=regionBBoxPoly, weight = 2, fill=FALSE, layerId ="myBBoxLayer")%>%
      addScaleBar()%>%
      onRender(
        "function(el,x){
                    this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lat, lng];
                        Shiny.onInputChange('hover_coordinates', coord)
                    });
                    this.on('mouseout', function(e) {
                        Shiny.onInputChange('hover_coordinates', null)
                    })
                }"
      )
    
    
    
  })
  ########################################################################################################
  
  
  
  ########################################################################################################
  #next we use the observe function to make the checkboxes dynamic. 
  #If you leave this part out you will see that the checkboxes, when clicked on the 
  #first time, display our filters...But if you then uncheck them they stay on. 
  #So we need to tell the server to update the map when the checkboxes are unchecked.
  ########################################################################################################
  #startPoly button
  observe({
    proxy <- leafletProxy("mymap")
    #proxy %>% clearMarkers()
    if (input$startPoly) {
      polyBEGIN <<- TRUE
      bsBEGIN <<- FALSE
      btBEGIN <<- FALSE
      
      disable("rndBoats")
      disable("rnd_boat_number")
      
      disable("deployTriDepots")
      disable("deploySqDepots")
      
      disable("rescueRedGray1by1")
      disable("rescueGray1by1")
      disable("permRedGray")
      disable("permGray")
      disable("clookRedGray")
      disable("clookGray")
      disable("gaPermRedGray")
      disable("gaSectorRedGray")
      
      enable("closePolygon")
      
      output$myMsg <- renderText({paste("Select at least 3 pts for the region\n")})
      #regionPolyDF <<- NULL
      #regionVtxMtx <<- NULL
      
      #BSs
      BSList <<- list()
      BSMtx <<- NULL
      nBS <<- 0
      
      #Boats
      nBT <<- 0 
      BTList <<- list()
      BTMtx <<- NULL
      BT2DTMtx <<- NULL
      
      
      #depots
      nDT <<- 0
      DTList <<- list()
      DTMtx <<- NULL
      D2DlinesMTX <<- NULL 
      depotPolygons <<- st_polygon(list())
      
      
      #Total BS and Depots
      nBSandDepot <<- nBS + nDT
      
      
    }else {
      polyBEGIN <<- FALSE
      #output$myMsg <- renderText({paste("Toggle start poly:FALSE\n")})
      mapToSave$current <- mapToSave$base %>% clearControls()
      # proxy %>% clearMarkers() %>% clearControls()
      proxy %>% clearControls()
      
      #cat("DEBUG: regionPolyDF cleared\n")
      #regionPolyDF <<- NULL
      #regionVtxMtx <<- NULL
      BSList <<- list()
      BSMtx <<- NULL
      nBS <<- 0
      
      #depots
      nDT <<- 0
      DTList <<- list()
      DTMtx <<- NULL
      D2DlinesMTX <<- NULL
      depotPolygons <<- st_polygon(list())
      
      
      #Total BS and Depots
      nBSandDepot <<- nBS + nDT
    }
  })
  ########################################################################################################
  
  
  
  ########################################################################################################
  #startBS button
  observe({
    proxy <- leafletProxy("mymap")
    #proxy %>% clearMarkers()
    if (input$startBS) {
      bsBEGIN <<- TRUE
      polyBEGIN <<- FALSE
      disable("closePolygon")
      
      #TODO
      #enable("findCov")
      #enable("showVoronoi")
      
      output$myMsg <- renderText({paste("Select BS pos inside polygon\n")})
      
      #regionPolyDF <<- NULL
      #regionVtxMtx <<- NULL
      BSList <<- list()
      BSMtx <<- NULL
      nBS <<- 0
      
      #depots
      nDT <<- 0
      DTList <<- list()
      DTMtx <<- NULL
      D2DlinesMTX <<- NULL
      depotPolygons <<- st_polygon(list())
      
      
      #Total BS and Depots
      nBSandDepot <<- nBS + nDT
      
      
    }else {
      bsBEGIN <<- FALSE
    }
  })
  ########################################################################################################
  
  
  
  # ######################################################################################################## 
  # observeEvent(input$myGraphScale,{
  #   #When the scale changes update the plot
  #   myCoords <- input$myGraphScale * DTMtx
  #   cat("Scale changed:",input$myGraphScale ,"\n")
  #   
  #   
  #   # output$iPlot <- renderPlot({
  #   ####################################################################
  #   #BEGIN iPlot stuff
  #   ####################################################################
  #   if (!(is.null(myGraph$value))){
  #     cat("Plotting new graph.\n")
  #     mylayout <- layout_nicely(myGraph$value,dim = 2)
  #     par(mar=c(0,0,0,0))
  #     svgPanZoom(
  #       svglite:::inlineSVG(
  #     plot(myGraph$value, vertex.size=7, edge.label = E(myGraph$value)$weight, layout=myCoords)), 
  #     controlIconsEnabled = T)
  #   }else{
  #     cat("The graph is null\n")
  #   }
  #   
  #   
  #   
  #   ####################################################################
  #   #END iPlot stuff
  #   ####################################################################
  #   
  #   #}) # end of output$iPlot <- renderPlot
  # })
  # ######################################################################################################## 
  
  
  
  ######################################################################################################## 
  
  # output$main_plot <- renderSvgPanZoom({
  #   p <- ggplot() + geom_point(data=data.frame(faithful),aes(x=eruptions,y=waiting)) + stat_density2d(data=data.frame(faithful),aes(x=eruptions,y=waiting, alpha =..level..),geom="polygon") + scale_alpha_continuous(range=c(0.05,0.2))
  #   svgPanZoom(p, controlIconsEnabled = T)
  # })
  
  
  
  
  output$iPlot <- renderSvgPanZoom({
    ####################################################################
    #BEGIN iPlot stuff
    ####################################################################
    if (!(length(E(myGraph$value)) == 0)){
      #myCoords <- myGraphScale * VTXMtx
      myCoords <- 20 * VTXMtx
      #DEBUG cat("***Scale changed:",input$myGraphScale ,"\n")
      
      
      cat("***Plotting new graph with",length(E(myGraph$value)),"edges.\n")
      cat("nDT, nTriDT, nSqDT=",nDT, nTriDT, nSqDT, "nBT, TrinBT, SqnBT=",nBT, TrinBT, SqnBT,"\n")
      mylayout <- layout_nicely(myGraph$value,dim = 2)
      par(mar=c(0,0,0,0))
      E(myGraph$value)$label.cex <<- 0.15
      
      #ggnet2(net, node.size = 6, node.color = "black", edge.size = 1, edge.color = "grey")
      #ggnet2(net, node.size = 6, node.color = "black", edge.size = 1, edge.color = "grey")
      # p <- GGally::ggnet2(myGraph$value, node.size = 7, 
      #                     label = V(myGraph$value)$label, label.size = 2, 
      #                     edge.label = round((E(myGraph$value)$weight/15000),nPrecDigit), 
      #                     edge.label.color = E(myGraph$value)$color, edge.label.size = 2
      #                     )
      # svgPanZoom(p,   controlIconsEnabled = T)
      
      svgPanZoom(
        #viewBox = FALSE, 
        #width = "auto", height = "auto",
        width="1000px", height="540px",
        svglite:::inlineSVG(
          #show(plot(myGraph$value, vertex.size=7, edge.label = E(myGraph$value)$weight, layout=myCoords))),
          show(plot(myGraph$value, vertex.size=4, edge.label = round((E(myGraph$value)$weight/input$drone_range),nPrecDigit), layout=myCoords)))
        , controlIconsEnabled = T)
      
    }else{
      cat("No edge to draw!\n")
    }
    
    
    ####################################################################
    #END iPlot stuff
    ####################################################################
    
    
  }) # end of output$iPlot <- renderPlot
  
  ######################################################################################################## 
  
  
  
  
  
  
  
  ######################################################################################################## 
  observe({
    #Mouse click
    
    click <- input$mymap_click
    if(is.null(click)) return()
    text<-paste("Lat:", round(click$lat,nPrecDigit), "Lng:", round(click$lng,nPrecDigit))
    text2<-paste0("Clicked", click$id, "at: ",text)
    
    #Check the mode
    if (polyBEGIN == TRUE){
      regionVtxMtx <<- rbind(regionVtxMtx, c(click$lng, click$lat))
      text2<-paste0("Polygon point[", length(regionVtxMtx[,1]),"] ", click$id, "at: ",text)
      cat("Add poly point - Total points:", length(regionVtxMtx[,1]),"\n")
      isolate({
        new_zoom <- 9
        if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
        #paste("Cleared Selection: Lat:", round(input$mymap_center$lat,nPrecDigit),"Lng:", round(input$mymap_center$lng,nPrecDigit),"Zoom:",new_zoom, "\n")
        output$myMsg <- renderText({paste("Click Selection: Lat:", round(click$lat,nPrecDigit),"Lng:", round(click$lng,nPrecDigit), "Zoom:",new_zoom, "\n") })
        #Remove the previous circle layer and add the new update one
        
        #leafletProxy("mymap") %>% removeShape(c("regionVtxMarkers","regionEdgeMarkers")) %>% clearMarkers()
        #mapToSave$current <- mapToSave$base %>% removeShape(c("regionVtxMarkers","regionEdgeMarkers")) %>% clearMarkers()
        
        leafletProxy("mymap") %>% 
          addCircles(data = as.data.frame(regionVtxMtx),
                     lng = ~ V1, 
                     lat = ~ V2,
                     color = "red",
                     radius = 10,
                     stroke = TRUE, 
                     opacity = 5,
                     weight = 1,
                     fillColor = "red",
                     fillOpacity = 0.5) %>% 
          addPolylines(data = as.data.frame(regionVtxMtx),
                       lng = ~ V1, 
                       lat = ~ V2,
                       color = "blue",
                       weight = 3)
        
        mapToSave$current <- mapToSave$base %>% 
          addCircles(data = as.data.frame(regionVtxMtx),
                     lng = ~ V1, 
                     lat = ~ V2,
                     color = "red",
                     radius = 10,
                     stroke = TRUE, 
                     opacity = 5,
                     weight = 1,
                     fillColor = "red",
                     fillOpacity = 0.5) %>% 
          addPolylines(data = as.data.frame(regionVtxMtx),
                       lng = ~ V1, 
                       lat = ~ V2,
                       color = "blue",
                       weight = 3)
        
        #addPolygons(data=regionPolyDF, weight = 2, fillColor = "red", popup=popup,fillOpacity = 0.2, layerId ="myPolyLayer")
        #setView(lng = input$lng, lat = input$lat, zoom = new_zoom)
      })
      
      #mymap$clearPopups()
      #mymap$showPopup( click$lat, click$lng, text)
    }else if (bsBEGIN == TRUE){
      #TODO check if the point is outside of the poly here - Automatic?
      
      
      
      #here check if the point is in the regionPoly!!!!
      
      if (point.in.polygon(click$lng, click$lat, regionPoly[[1]][,1], regionPoly[[1]][,2]) != 0){
        
        nBS <<- nBS + 1
        nBSandDepot  <<- nBSandDepot + 1
        
        
        enable("deployTriDepots")
        enable("deploySqDepots")
        
        text2<-paste0("BS point[", nBS,"] ", click$id, "at: ",text)
        #DEBUG
        cat("BS point[", nBS,"] ", click$id, "at: ",text,"\n")
        cat("Clicked point is in the region. Added BS point - Total points:", nBS,"\n")
        
        
        
        
        ###############################################
        #UNCOMMENT ONE OF THE FOLLOWING:
        ###############################################
        
        
        ###############################################
        # 1-) ADD BS ALWAYS TO THE CLICKED PT
        ###############################################
        # BSList[[nBS]] <<- st_point(c(click$lng, click$lat))
        # BSMtx <<- rbind(BSMtx, c(click$lng, click$lat))
        
        
        
        
        ###############################################
        # 2-) ADD BS ALWAYS TO THE SAME PT FOR EXPERIMENTS
        ###############################################
        BSList[[nBS]] <<- st_point(c(13.5, 43.631))
        BSMtx <<- rbind(BSMtx, c(13.5, 43.631))
        
        ###############################################
        
        
        
        isolate({
          new_zoom <- 9
          if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
          output$myMsg <- renderText({paste("Click Selection: Lat:", round(click$lat,nPrecDigit),"Lng:", round(click$lng,nPrecDigit), "Zoom:",new_zoom, "\n") })
          
          #Remove the previous circle layer and add the new update one
          #leafletProxy("mymap") %>% removeShape("myBSEdges") %>% clearMarkers()
          #mapToSave$current <- mapToSave$base %>% removeShape("myBSEdges") %>% clearMarkers("myBSPts")
          
          
          leafletProxy("mymap") %>% 
            addMarkers(data = as.data.frame(BSMtx),
                       lng = ~ V1, 
                       lat = ~ V2,
                       icon = myBSIcon)  
          
          # %>% 
          #   addPolylines(data = as.data.frame(BSMtx),
          #                lng = ~ V1, lat = ~ V2,
          #                color = "green",
          #                weight = 3)
          
          
          
          
          mapToSave$current <- mapToSave$base %>% 
            addMarkers(data = as.data.frame(BSMtx),
                       lng = ~ V1, 
                       lat = ~ V2,
                       icon = myBSIcon)  
          
          # %>% 
          #   addPolylines(data = as.data.frame(BSMtx),
          #                lng = ~ V1, lat = ~ V2,
          #                color = "green",
          #                weight = 3)
          
          #addPolygons(data=regionPolyDF, weight = 2, fillColor = "red", popup=popup,fillOpacity = 0.2, layerId ="myPolyLayer")
          #setView(lng = input$lng, lat = input$lat, zoom = new_zoom)
        })
        
      }else{
        cat("Clicked point outside of the region!!!\n")
      }
    }else if (btBEGIN == TRUE){
      
      
      
      #here check if the point is in the regionPoly!!!!
      #ATTENTION If the user is stupid and keeps clicking the same point
      #you should not add thousands of boats!!!!
      
      if (point.in.polygon(click$lng, click$lat, regionPoly[[1]][,1], regionPoly[[1]][,2]) != 0){
        
        #Check if it is reachable!!!
        boatPos <- as.data.frame(cbind(click$lng, click$lat)) 
        colnames(boatPos) <- c("x","y")
        
        if (checkIfAllBoatsReachableForGrids(boatPos) == TRUE) {
          
          currentDroneRange <- reac()$drone_range
          # nBT <<- nBT + 1
          # nVertex <<- nBS + nDT + nBT
          #DEBUG
          cat("Drone range",currentDroneRange," meters. In polygon Vertex no:",nVertex,"Boat No:",nBT,"\n")
          
          #LATER nBSandDepot  <<- nBSandDepot + 1
          
          enable("rescueRedGray1by1")
          enable("rescueGray1by1")
          enable("permRedGray")
          enable("permGray")
          enable("clookRedGray")
          enable("clookGray")
          enable("gaPermRedGray")
          enable("gaSectorRedGray")
          
          text2 <- paste0("BT point[", nBT,"] ", click$id, "at: ",text)
          
          cat("Clicked point is in the region. Added BT point - Total points:", nBT,"\n")
          
          
          
          
          #BTMtx <<- rbind(BTMtx, c(click$lng, click$lat))
          #VTXMtx <<- rbind(VTXMtx,  c(click$lng, click$lat))
          
          
          ##################################################################################################################
          #Boat add and show BEGIN
          ##################################################################################################################
          isolate({
            new_zoom <- 9
            if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
            output$myMsg <- renderText({paste("Click Selection: Lat:", round(click$lat,nPrecDigit),
                                              "Lng:", round(click$lng,nPrecDigit), "Zoom:",new_zoom, "\n") })
            
            addBoatSq(boatPos, 1)
            addBoatTri(boatPos, 1)
            
            addAndShowBoatsActive()
            
            cat("************* Single Boat added and displayed\n")
            
            
          })
          ##################################################################################################################
          #Boat add and show END
          ##################################################################################################################
        }
        else {
          text2 <- paste("Boat not reachable at", click$id,text)
          cat("Boat not reachable at", click$id,text,"\n")
        }
      }else{
        text2<-paste("Clicked point", click$id,text)
        #mymap$clearPopups()
        #mymap$showPopup( click$lat, click$lng, text)
      }
    }
    output$clickMsg<-renderText({ text2 })
  })
  
  ########################################################################################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Set this to "force" instead of TRUE for testing locally (without Shiny Server)
  session$allowReconnect(TRUE)
  
  
  
  
  
  
  
  
  
  
  
  
}
########################################################################################################

