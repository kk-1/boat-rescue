


########################################################################################################
# Begin Functions
########################################################################################################



########################################################################################################
#Given lon lat in a data frame sort points
#https://raw.githubusercontent.com/skgrange/gissr/master/R/sort_points.R
########################################################################################################

my_sort_points <- function(df, y = "latitude", x = "longitude", clockwise = TRUE, bsx, bsy) {
  #we need to define delta sector
  #if any two points fall into same sector
  # we need to give the one that is closest to the "last point" visited
  # NA check, if NAs drop them
  if (any(is.na(c(df[, y], df[, x])))) {
    
    # Remove NAs
    df <- df[!(is.na(df[, y]) & is.na(df[, x])), ]
    
    # Raise warning
    warning("Missing coordinates were detected and have been removed.", 
            call. = FALSE)
    
    # Check 
    if (nrow(df) == 0) stop("There are no valid coordinates.", call. = FALSE)
    
  }
  
  # Get centre (-oid) point of points
  #x_centre <- mean(df[, x])
  #y_centre <- mean(df[, y])
  
  x_centre <- bsx
  y_centre <- bsy
  
  
  # Calculate deltas
  df$x_delta <- df[, x] - x_centre
  df$y_delta <- df[, y] - y_centre
  
  # Resolve angle, in radians
  df$angle <- atan2(df$y_delta, df$x_delta)
  df$angle_degrees <- df$angle * 180 / pi
  
  # Arrange by angle
  if (clockwise) {
    
    df <- df[order(df$angle, decreasing = TRUE), ]
    
  } else {
    
    df <- df[order(df$angle, decreasing = FALSE), ]
    
  }
  
  # Drop intermediate variables
  #df[, c("x_delta", "y_delta", "angle")] <- NULL
  
  # Return
  df
  
}

########################################################################################################
#Given lon lat in a 2d list creates spatial polygons data frame
########################################################################################################
VtxMtx2SpatPolyDF <- function(xVtxMtx){
  
  #make polygon from kml
  p1 <- sp::Polygon(xVtxMtx)
  #make Polygon class
  p2 <- sp::Polygons(list(p1), ID = "drivetime")
  #make spatial polygons class
  p3 <- sp::SpatialPolygons(list(p2),proj4string=sp::CRS("+init=epsg:4326"))
  # Create a dataframe and display default rownames
  p3.df <- data.frame(ID=1:length(p3)) 
  rownames(p3.df)
  # Extract polygon ID's
  pid <- sapply(slot(p3, "polygons"), function(x) slot(x, "ID")) 
  # Create dataframe with correct rownames
  p3.df <- data.frame( ID=1:length(p3), row.names = pid)    
  #Create the SpatialPolygonsDataFrame
  p4 <- SpatialPolygonsDataFrame(p3, data=p3.df)
  
  return(p4)
}
########################################################################################################


########################################################################################################
#Given the n and the size of the region generates boats uniformly
########################################################################################################
my_generate_boats <- function(n, width, height) {
  
  x<-sample(1:width,n)
  y<-sample(1:height,n)
  
  xy <- cbind(x,y)
  
  return (xy)
  
}
########################################################################################################




###############################################################################################################################
#https://stackoverflow.com/questions/31051133/how-do-i-make-sure-that-a-shiny-reactive-plot-only-changes-once-all-other-reacti
###############################################################################################################################
# Redefined in global namespace since it's not exported from shiny
###`%OR%` <- shiny:::`%OR%`
# Gives error:
#Error in get(name, envir = asNamespace(pkg), inherits = FALSE) : 
#  object '%OR%' not found
debounce_sc <- function(r, millis, priority = 100, domain = getDefaultReactiveDomain(), short_circuit = NULL) 
{
  force(r)
  force(millis)
  if (!is.function(millis)) {
    origMillis <- millis
    millis <- function() origMillis
  }
  v <- reactiveValues(trigger = NULL, when = NULL)
  firstRun <- TRUE
  observe({
    r()
    if (firstRun) {
      firstRun <<- FALSE
      return()
    }
    v$when <- Sys.time() + millis()/1000
  }, label = "debounce tracker", domain = domain, priority = priority)
  # New code here to short circuit the timer when the short_circuit reactive
  # triggers
  if (inherits(short_circuit, "reactive")) {
    observe({
      short_circuit()
      v$when <- Sys.time()
    }, label = "debounce short circuit", domain = domain, priority = priority)
  }
  # New code ends
  observe({
    if (is.null(v$when)) 
      return()
    now <- Sys.time()
    if (now >= v$when) {
      v$trigger <- isolate(v$trigger %OR% 0) %% 999999999 + 
        1
      v$when <- NULL
    }
    else {
      invalidateLater((v$when - now) * 1000)
    }
  }, label = "debounce timer", domain = domain, priority = priority)
  er <- eventReactive(v$trigger, {
    r()
  }, label = "debounce result", ignoreNULL = FALSE, domain = domain)
  primer <- observe({
    primer$destroy()
    er()
  }, label = "debounce primer", domain = domain, priority = priority)
  er
}
########################################################################################################

########################################################################################################
#https://stackoverflow.com/questions/639695/how-to-convert-latitude-or-longitude-to-meters
#https://en.wikipedia.org/wiki/Haversine_formula
#Converts lon, lat to meters
########################################################################################################
lonlat2m <- function(lon1,lat1,lon2,lat2) {
  R <- 6378.137                                # radius of earth in Km
  dLat <- (lat2-lat1)*pi/180
  dLon <- (lon2-lon1)*pi/180
  a <- sin((dLat/2))^2 + cos(lat1*pi/180)*cos(lat2*pi/180)*(sin(dLon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R * c
  return (d * 1000)                            # distance in meters
}
########################################################################################################



########################################################################################################
#Generates depots and fills global arrays
#FOR SINGLE BS CURRENTLY
########################################################################################################
my_generate_depots<- function(){
  
  cat("Generating depots\n")
  
}
########################################################################################################


########################################################################################################
#https://bookdown.org/robinlovelace/geocompr/reproj-geo-data.html
#To calculate the EPSG code associated with any point on the planet as follows:
########################################################################################################

lonlat2UTM = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}
########################################################################################################





########################################################################################################
# THIS FUNCTION ASSUMEs THAT EVERYTHING IS CONNECTED!!!
#
# This function returns the metrics  of the path in a list given the perm of boat numbers (not vertex ids!!!)
# The list elements:
#  theResult[[1]] : Path length in meters
#  theResult[[2]] : Path length in hops
#  theResult[[3]] : AVG waiting distance in meters
#  theResult[[4]] : Number of chargings
#  theResult[[5]] : The path string for printing
#  theResult[[6]] : The path for coloring
#  theResult[[7]] : The path len for coloring

##########################################################################################################
my_get_metrics_permDEBUG <- function(gInput, boatPerm, currentDroneRange) {
  
  #DEBUG
  # gInput <- gGray
  # boatPerm <- 1
  # currentDroneRange <- input$drone_range
  
  #get the vector showing the order of rescue (permutation)
  rescueOrd <- boatPerm + (nDT + nBS)
  
  printOrd <- sprintf("%s%d","B",(rescueOrd - (nDT + nBS)))
  
  #The following has to be list of list
  #Permutations and
  #For each permutation for each sp (boat to boat)
  PermPathList <- list()
  PermPathListLen <- 0
  
  totPathLen <- 0
  totPathCost <- 0
  totPath <- ""
  
  text2print <- paste("Trying:",paste(printOrd,collapse = " -> "),":\n")
  
  #DEBUG
  cat("***********************************BEGIN my_get_metrics_permDEBUG**************************************\n")
  
  cat("Perm vtx ids are:", rescueOrd,"Boat ids:", paste(printOrd,collapse = " -> "),"\n")
  #cat(text2print)
  
  ####################
  #ATTENTION:
  ####################
  #For the given boat permutation
  #sometimes in between the sp part may go through a boat
  #For brute force no problem. It means the current perm is not the best one!
  #However for other algorithms it should be checked!!!!
  #OR
  #When you analyse path between BS-BT and BT-BT
  #You just "hide" all the other BTs
  ####################
  
  ##########################################################################################################
  #1/3 --- Start from the BS to first boat
  ##########################################################################################################
  v1 <- 1 #BS
  v2 <- rescueOrd[1] #vertex no of the first boat 
  
  #"hide" unrelated boats to force the permutation!!!
  vtx2HideSet <- rescueOrd[-1]
  cat("Hiding",vtx2HideSet,"for",v2,"\n")
  gx <- delete.vertices(gInput,vtx2HideSet)
  
  #DEBUG 
  cat("my_get_metrics_permDEBUG 1/3 --- Trying sp from",v1,"to",v2,"\n")
  rescueSP <-  shortest_paths(gx, from = match(v1, V(gx)$name), to = match(v2, V(gx)$name), output="both")
  
  #Convert the rescueSP to path on the gInput for standardization
  #After new gx is created all the stuff on the old gx are deleted!!!
  rescueSP$epath[[1]] <- E(gInput,path = names(rescueSP$vpath[[1]]))
  rescueSP$vpath[[1]] <- V(gInput)[rescueSP$vpath[[1]]$name]
  
  #DEBUG cat("OK SP\n")
  
  
  #After each sp check if the last edge is red or not
  #if there is a red edge at the end then for sure there is a gray edge around
  #find the gray edges that sum up to drone range with the red edge and
  #try each gray edge dst vertex (src is the current boat) 
  #to find sp to the next boat. Pick the min length sp
  
  #########################
  #BEGIN sp analysis
  #########################
  minsp <- Inf
  bestGrayDepotId <- -1
  splen <- length(rescueSP$epath[[1]])
  if (rescueSP$epath[[1]][splen]$color == redEdgeCol) {
    #DEBUG
    cat("my_get_metrics_permDEBUG 1/3 --- FROM BS: Red edge from",rescueSP$vpath[[1]][splen]$label,"to",v2,"\n")
    
    
    
    #Before we search the best graydepot we can check here to see if they are yellow edges to other
    #boats in the vicinity. These boats can be in the same depot "triangle" or "rectangle"
    #Now we find a path (yellow edges + gray edge from the last boat) <= (drone range - red edge)
    
    #yes we have red edge in sp
    #Find the best gray edge out of the Boat at the end of the sp
    #nGrayEdges <- sum(BT2DTGrayEdge[(v2 - nBSandDepot),])
    GrayEdgeIdx <- which(BT2DTGrayEdge[(v2 - nBSandDepot),] == 1)
    if (nBT == 1) {
      nextVtx <- 1
      gx2 <- gx
    }else{
      nextVtx <- rescueOrd[2]
      vtx2HideSet <- rescueOrd[-2]
      gx2 <- delete.vertices(gInput,vtx2HideSet)
    }
    for (m in GrayEdgeIdx) {
      cat("my_get_metrics_permDEBUG 1/3 --- Testing gray edge idx",m,"\n")
      if ((v2 - nBSandDepot) < 0) {cat(m,"- my_get_metrics_permDEBUG - 1/3 ******************************** ERROR: v2:",v2,"not a boat id!\n")}
      if ( (rescueSP$epath[[1]][splen]$weight + BT2DTMtx[(v2 - nBSandDepot), m]) <= currentDroneRange ){
        #Good gray edge lets try to find sp from the dest depot to the next boat
        GrayDepotSP <-  shortest_paths(gx2, from = match(m, V(gx2)$name), to = match(nextVtx, V(gx2)$name), output="both")
        #CostGrayDepotSP <- sum(E(gx2, path=unlist( GrayDepotSP$vpath))$weight)
        CostGrayDepotSP <- sum(E(gInput, path=c(names(GrayDepotSP$vpath[[1]])))$weight)
        
        #Getting values from another graph by using names example below:
        #CostGrayDepotSP <- sum(E(gInput, path=c(names(GrayDepotSP$vpath[[1]])))$weight)
        if (CostGrayDepotSP < minsp ) {
          minsp <- CostGrayDepotSP
          bestGrayDepotId <- m
          bestGrayDepotSP <- GrayDepotSP
        }
      }
    }#end for
  }# if red edge at the end of sp
  
  #########################
  #END sp analysis
  #########################
  
  
  
  if (bestGrayDepotId == -1) {
    #The sp ends on a boat
    #DEBUG
    cat("my_get_metrics_permDEBUG 1/3 --- FROM BS: No red edge\n")
    #The last edge of the first sp is not red edge so normal stuff
    PermPathListLen <- PermPathListLen + 1
    #PermPathList[[PermPathListLen]]  <-  rescueSP
    PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
    
    pLen <- length(rescueSP$vpath[1][[1]]) -1
    totPathLen <-  totPathLen + pLen
    #totPathCost <- totPathCost + sum(E(gx, path=unlist(rescueSP$vpath))$weight)
    totPathCost <- totPathCost + sum(E(gInput, path=c(names(rescueSP$vpath[[1]])))$weight)
    
    
    pStr <- rescueSP$vpath[1][[1]]
    #DEBUG cat(pStr,"\n")
    #DEBUG 
    cat("my_get_metrics_permDEBUG 1/3 --- From BS to first Boat:",pStr,"\n")
    
    totPath <- rescueSP$vpath[1][[1]]$label
    pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
    text2print <- paste(text2print,"Shortest rescue path from BS to", 
                        sprintf("%s%d","B",(v2 - (nDT + 1))),
                        "len =",pLen,":", pStr,"\n")
    #DEBUG 
    #cat("***>",text2print)
    
  }else{
    #The sp ends on a depot via gray edge!!!
    #DEBUG
    cat("my_get_metrics_permDEBUG 1/3 --- FROM BS: Red edge with gray depot\n")
    #Now we got a red edge so the last point is depot not a boat
    #Normal stuff but add the depot to the rescueSP
    
    #PROBLEM HERE adding from gx or gInput: In the PermPathList the vertices of the path can not be extracted
    #when vertex or edge is added like the one below?
    
    #Add the depot vertex and the red edge
    #DEBUG
    cat("my_get_metrics_permDEBUG 1/3 --- FROM BS: Adding vertex", bestGrayDepotId,"as Gray Depot vtx",V(gInput)[bestGrayDepotId],"\n")
    rescueSP$vpath[[1]] <- c(rescueSP$vpath[[1]] ,V(gInput)[bestGrayDepotId])
    #DEBUG
    cat("my_get_metrics_permDEBUG 1/3 --- FROM BS: Adding edge", v2,"--", bestGrayDepotId,"\n")
    rescueSP$epath[[1]] <- c(rescueSP$epath[[1]] ,E(gInput, c(v2, bestGrayDepotId)))
    #DEBUG 
    cat("my_get_metrics_permDEBUG 1/3 --- Add vertex+edge is OK!\n")
    
    PermPathListLen <- PermPathListLen + 1
    #PermPathList[[PermPathListLen]]  <-  rescueSP
    PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
    
    pLen <- length(rescueSP$vpath[1][[1]]) -1
    totPathLen <-  totPathLen + pLen
    #totPathCost <- totPathCost + sum(E(gInput, path=unlist(rescueSP$vpath))$weight)
    totPathCost <- totPathCost + sum(E(gInput, path=c(names(rescueSP$vpath[[1]])))$weight)
    
    pStr <- rescueSP$vpath[1][[1]]
    #DEBUG cat(pStr,"\n")
    #DEBUG 
    cat("my_get_metrics_permDEBUG 1/3 --- From BS to first Boat -> Gray Depot:",pStr,"\n")
    
    totPath <- rescueSP$vpath[1][[1]]$label
    pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
    text2print <- paste(text2print,"Shortest rescue path with red edge from BS to", 
                        sprintf("%s%d","B",(v2 - (nDT + 1))),
                        "len =",pLen,":", pStr,"\n")
    #DEBUG 
    #cat("***>",text2print)
    #Now update the src of the next path as the depot we reach with gray edge
    #WATCH OUT V2 is not boat id anymore!!!
    v2 <- bestGrayDepotId
  }#else we got a red edge
  
  ##########################################################################################################
  #2/3 --- from the first boat jump to next till the last one when we have more than 1 boat
  ##########################################################################################################
  if (nBT > 1) {
    
    for (b in 2:nBT){
      v1 <- v2 #If previous path has red-gray pair then this v1 will be depot id not boat id!!! 
      v2 <- rescueOrd[b]
      
      
      #"hide" unrelated boats to force the permutation!!!
      #please check if v1 and v2 are boats
      if (v1>(nDT + nBS)) {
        #v1 is also a boat so do not hide it too
        vtx2HideSet <- rescueOrd[-c(b-1,b)]
      }else{
        #v1 is depot hide other boats leave v2
        vtx2HideSet <- rescueOrd[-b]
      }
      cat("Hiding",vtx2HideSet,"for",v2,"\n")
      gx <- delete.vertices(gInput,vtx2HideSet)
      
      
      #DEBUG 
      cat("my_get_metrics_permDEBUG 2/3 --- Trying sp from",v1,"to",v2,"\n")
      rescueSP <-  shortest_paths(gx, from = match(v1, V(gx)$name), to = match(v2, V(gx)$name), output="both")
      
      #Convert the rescueSP to path on the gInput for standardization
      #After new gx is created all the stuff on the old gx are deleted!!!
      rescueSP$epath[[1]] <- E(gInput,path = names(rescueSP$vpath[[1]]))
      rescueSP$vpath[[1]] <- V(gInput)[rescueSP$vpath[[1]]$name]
      
      #After each sp check if the last edge is red or not
      #if there is a red edge at the end then for sure there is a gray edge around
      #find the gray edges that sum up to drone range with the red edge and
      #try each gray edge dst vertex (src is the current boat) 
      #to find sp to the next boat. Pick the min length sp
      
      #########################
      #BEGIN sp analysis
      #########################
      minsp <- Inf
      bestGrayDepotId <- -1
      splen <- length(rescueSP$epath[[1]])
      if (rescueSP$epath[[1]][splen]$color == redEdgeCol) {
        #DEBUG
        cat("my_get_metrics_permDEBUG 2/3 --- BETWEEN BOATS: Red edge from",rescueSP$vpath[[1]][splen]$label,"to",v2,"\n")
        #yes we have red edge in sp
        #Find the best gray edge out of the Boat at the end of the sp
        #nGrayEdges <- sum(BT2DTGrayEdge[(v2 - nBSandDepot),])
        GrayEdgeIdx <- which(BT2DTGrayEdge[(v2 - nBSandDepot),] == 1)
        
        #Here we can have BS as the last vertex in the tour as bestGrayDepot
        #In this case if we can reach BS then it is the best one no need to go other depots
        #However if we still have other boats to rescue than we need to go to the min gray depot
        #to save some distance
        
        #This is a special condition:
        #BS is closer than the dronerange
        #We are at the last Boat
        #BS and last boat has a gray edge
        if ( ((rescueSP$epath[[1]][splen]$weight + BT2DTMtx[(v2 - nBSandDepot), 1]) <= currentDroneRange ) && 
             (v2 == rescueOrd[nBT]) && (is.element(1,GrayEdgeIdx)) ) {
          
          #  if (is.element(1,GrayEdgeIdx)) {
          
          cat("my_get_metrics_permDEBUG 2/3 --- We got BS as the reachable last stop from the last boat",v2,"via gray edge\n")
          #The last boat to be rescued
          #From that boat if we can reach to BS via red or gray just take it
          #On phase 2/3 we can reach BS from gray edge added before
          # if there is one just take it
          
          #ATTENTION rescueOrd[2] correct?
          if (b == nBT) {
            #The next "boat" is the BS
            nextVtx <- 1
            gx2 <- gx
          } else {
            nextVtx <- rescueOrd[(b+1)]
            vtx2HideSet <- rescueOrd[-(b+1)]
            gx2 <- delete.vertices(gInput,vtx2HideSet)
          }
          
          GrayDepotSP <-  shortest_paths(gx2, from = match(1, V(gx2)$name), to = match(nextVtx, V(gx2)$name), output="both")
          
          #GrayDepotSP <-  shortest_paths(gx, from = match(1, V(gx)$name), to = match(rescueOrd[2], V(gx)$name), output="both")
          #CostGrayDepotSP <- sum(E(gInput, path=unlist(GrayDepotSP$vpath))$weight)
          CostGrayDepotSP <- sum(E(gInput, path=c(names(GrayDepotSP$vpath[[1]])))$weight)
          
          
          
          minsp <- CostGrayDepotSP
          bestGrayDepotId <- 1
          bestGrayDepotSP <- GrayDepotSP
          
          
        }else{
          #Now we know that BS is not among grayDepots that can be chosen for the last boat
          for (m in GrayEdgeIdx) {
            cat("my_get_metrics_permDEBUG 2/3 --- Testing gray edge idx",m,"\n")
            if ((v2 - nBSandDepot) < 0) {
              cat(m,"- my_get_metrics_permDEBUG - 2/3 ******************************** ERROR: v2:",v2,"not a boat id!\n")
            }
            if ((rescueSP$epath[[1]][splen]$weight + BT2DTMtx[(v2 - nBSandDepot), m]) <= currentDroneRange){
              #Good gray edge lets try to find sp from the dest depot to the next boat
              #ATTENTION rescueOrd[2] correct? NO
              
              
              if (b == nBT) {
                #The next "boat" is the BS
                nextVtx <- 1
                gx2 <- gx
              } else {
                nextVtx <- rescueOrd[(b+1)]
                vtx2HideSet <- rescueOrd[-(b+1)]
                gx2 <- delete.vertices(gInput,vtx2HideSet)
              }
              
              
              
              
              GrayDepotSP <-  shortest_paths(gx2, from = match(m, V(gx2)$name), to = match(nextVtx, V(gx2)$name), output="both")
              #CostGrayDepotSP <- sum(E(gInput, path=unlist(GrayDepotSP$vpath))$weight)
              CostGrayDepotSP <- sum(E(gInput, path=c(names(GrayDepotSP$vpath[[1]])))$weight)
              if (CostGrayDepotSP < minsp ) {
                minsp <- CostGrayDepotSP
                bestGrayDepotId <- m
                bestGrayDepotSP <- GrayDepotSP
              }
            }#end if test for drone range
          }#end for
        }#end else
        
        # }#if we are at the last boat to rescue....
        
      }# if red edge at the end of sp
      #########################
      #END sp analysis
      #########################
      
      
      
      if (bestGrayDepotId == -1) {
        #DEBUG
        cat("my_get_metrics_permDEBUG 2/3 --- BETWEEN BOATS: No red edge\n")
        #The last edge of the first sp is not red edge so normal stuff
        PermPathListLen <- PermPathListLen + 1
        #PermPathList[[PermPathListLen]]  <-  rescueSP
        PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
        
        pLen <- length(rescueSP$vpath[1][[1]]) -1
        totPathLen <-  totPathLen + pLen
        totPathCost <- totPathCost + sum(E(gInput, path=c(names(rescueSP$vpath[[1]])))$weight)
        
        
        #Getting values from another graph by using names example below:
        #CostGrayDepotSP <- sum(E(gInput, path=c(names(GrayDepotSP$vpath[[1]])))$weight)
        
        pStr <- names(rescueSP$vpath[[1]])
        #DEBUG cat(pStr,"\n")
        #DEBUG 
        cat("my_get_metrics_permDEBUG 2/3 --- From the first Boat to the others:",pStr,"\n")
        totPath <- rescueSP$vpath[1][[1]]$label
        pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
        
        if (v1 > nBSandDepot) { 
          #The last stop was a Boat
          srcID <- sprintf("%s%d","B",(v1 - (nDT + 1))) 
        }else {
          #The last stop was via Gray Depot
          srcID <- sprintf("%s%d","GD",v1) 
        }
        
        if (v2 > nBSandDepot) { 
          #The last stop was a Boat
          dstID <- sprintf("%s%d","B",(v2 - (nDT + 1))) 
        }else {
          #The last stop was via Gray Depot
          dstID <- sprintf("%s%d","GD",v2) 
        }
        
        
        text2print <- paste(text2print,"Shortest rescue path from", 
                            srcID,"to", dstID,"len =",pLen,":", pStr,"\n")
        
        
        #DEBUG 
        #cat("***>",text2print)
      }else{
        #DEBUG
        cat("my_get_metrics_permDEBUG 2/3 --- BETWEEN BOATS: Red edge\n")
        #Now we got a red edge so the last point is depot not a boat
        #Normal stuff but add the depot to the rescueSP
        
        #Add the depot vertex and the red edge
        #DEBUG
        cat("my_get_metrics_permDEBUG 2/3 --- Adding vertex", bestGrayDepotId,"\n")
        rescueSP$vpath[[1]] <- c(rescueSP$vpath[[1]] ,V(gInput)[bestGrayDepotId])
        cat("my_get_metrics_permDEBUG 2/3 --- Adding edge", v2,"--", bestGrayDepotId,"\n")
        rescueSP$epath[[1]] <- c(rescueSP$epath[[1]] ,E(gInput, c(v2, bestGrayDepotId)))
        #DEBUG cat("Add ok\n")
        
        PermPathListLen <- PermPathListLen + 1
        #PermPathList[[PermPathListLen]]  <-  rescueSP
        PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
        
        pLen <- length(rescueSP$vpath[1][[1]]) -1
        totPathLen <-  totPathLen + pLen
        #totPathCost <- totPathCost + sum(E(gInput, path=unlist(rescueSP$vpath))$weight)
        totPathCost <- totPathCost + sum(E(gInput, path=c(names(rescueSP$vpath[[1]])))$weight)
        
        pStr <- rescueSP$vpath[1][[1]]
        #DEBUG cat(pStr,"\n")
        #DEBUG 
        cat("my_get_metrics_permDEBUG 2/3 --- From the first Boat to the others with red edge:",pStr,"\n")
        totPath <- rescueSP$vpath[1][[1]]$label
        pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
        
        if (v1 > nBSandDepot) { 
          #The last stop was a Boat
          srcID <- sprintf("%s%d","B",(v1 - (nDT + 1))) 
        }else {
          #The last stop was via Gray Depot
          srcID <- sprintf("%s%d","GD",v1) 
        }
        
        if (v2 > nBSandDepot) { 
          #The last stop was a Boat
          dstID <- sprintf("%s%d","B",(v2 - (nDT + 1))) 
        }else {
          #The last stop was via Gray Depot
          dstID <- sprintf("%s%d","GD",v2) 
        }
        
        
        text2print <- paste(text2print,"Shortest rescue path with red edge from", 
                            srcID,"to",dstID,"len =",pLen,":", pStr,"\n")
        
        
        #DEBUG 
        #cat("***>",text2print)
        #Now update the src of the next path as the depot we reach with gray edge
        v2 <- bestGrayDepotId
      }#else we got a red edge
    }#for loop over boats
  } #if there are more than 1 boat
  
  ##########################################################################################################
  #3/3 --- from the last boat to BS back again
  ##########################################################################################################
  
  #May be we already reached BS from the previous phase
  if (v2 == 1) {
    #DEBUG 
    cat("my_get_metrics_permDEBUG 3/3 --- We already finished in 2/3\n")
  }else{
    v1 <- v2 #from the last boat CAN BE A DEPOT EVEN BS!!!!
    v2 <- 1 #back to BS
    
    
    #"hide" unrelated boats to force the permutation!!!
    #please check if v1 and v2 are boats
    if (v1>(nDT + nBS)) {
      #if we have more than 1 boat
      if (nBT > 1) {
      #v1 is also a boat so hide it too
      vtx2HideSet <- rescueOrd[-b] 
      cat("Hiding",vtx2HideSet,"for",v2,"\n")
      gx <- delete.vertices(gInput,vtx2HideSet)
      }
    }
    
    
    
    #DEBUG 
    cat("my_get_metrics_permDEBUG 3/3 --- Trying sp from",v1,"to",v2,"\n")
    rescueSP <-  shortest_paths(gx, from = match(v1, V(gx)$name), to = match(v2, V(gx)$name), output="both")
    
    #Convert the rescueSP to path on the gInput for standardization
    #After new gx is created all the stuff on the old gx are deleted!!!
    rescueSP$epath[[1]] <- E(gInput,path = names(rescueSP$vpath[[1]]))
    rescueSP$vpath[[1]] <- V(gInput)[rescueSP$vpath[[1]]$name]
    
    #After each sp check if the last edge is red or not
    #if there is a red edge at the end then for sure there is a gray edge around
    #find the gray edges that sum up to drone range with the red edge and
    #try each gray edge dst vertex (src is the current boat) 
    #to find sp to the next boat. Pick the min length sp
    
    #########################
    #BEGIN sp analysis
    #########################
    minsp <- Inf
    bestGrayDepotId <- -1
    splen <- length(rescueSP$epath[[1]])
    
    
    #The dest is 1 which is BS
    #So even if we have red edge at the end (This happen if the boat is close to the BS!!!)
    #No problem as long as its length is less than the drone range
    #The drone will charge to full at the BS!!!
    #HOWEVER IF THE DRONE IS COMING FROM A BOAT TO BS WE NEED TO BE SURE THAT
    #IT HAS ENOUGH ENERGY
    #
    
    
    if (rescueSP$epath[[1]][splen]$color == redEdgeCol) {
      #DEBUG
      cat("my_get_metrics_permDEBUG 3/3 --- BACK TO BS: Red edge from",rescueSP$vpath[[1]][splen]$label,"to",v2,"\n")
      PermPathListLen <- PermPathListLen + 1
      #PermPathList[[PermPathListLen]]  <-  rescueSP
      PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
      
      
      pLen <- length(rescueSP$vpath[1][[1]]) -1
      totPathLen <-  totPathLen + pLen
      #totPathCost <- totPathCost + sum(E(gInput, path=unlist(rescueSP$vpath))$weight)
      totPathCost <- totPathCost + sum(E(gInput, path=c(names(rescueSP$vpath[[1]])))$weight)
      
      pStr <- rescueSP$vpath[1][[1]]
      #DEBUG cat(pStr,"\n")
      #DEBUG
      cat("my_get_metrics_permDEBUG 3/3 --- From the last boat to BS back with red edge:",pStr,"\n")
      
      totPath <- rescueSP$vpath[1][[1]]$label
      pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
      if (v1 > nBSandDepot) { 
        #The last stop was a Boat
        srcID <- sprintf("%s%d","B",(v1 - (nDT + 1))) 
      }else {
        #The last stop was via Gray Depot
        srcID <- sprintf("%s%d","GD",v1 - (nDT + 1)) 
      }
      text2print <- paste(text2print,"Shortest rescue path with red edge from",
                          srcID,"to BS",
                          "len =",pLen,":", pStr,"\n")
      #DEBUG
      #cat("***>",text2printCW)
      
      # if red edge at the end of sp
    }else{
      #No red edge to BS
      cat("my_get_metrics_permDEBUG 3/3 --- BACK TO BS: No red edge from",rescueSP$vpath[[1]][splen]$label,"to",v2,"\n")
      PermPathListLen <- PermPathListLen + 1
      #PermPathList[[PermPathListLen]]  <-  rescueSP
      PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
      
      pLen <- length(rescueSP$vpath[1][[1]]) -1
      totPathLen <-  totPathLen + pLen
      #totPathCost <- totPathCost + sum(E(gx, path=unlist(rescueSP$vpath))$weight)
      totPathCost <- totPathCost + sum(E(gInput, path=c(names(rescueSP$vpath[[1]])))$weight)
      
      pStr <- names(rescueSP$vpath[[1]])
      #DEBUG cat(pStr,"\n")
      #DEBUG
      cat("my_get_metrics_permDEBUG 3/3 --- From the last boat to BS back without red edge:",pStr,"\n")
      
      totPath <- rescueSP$vpath[1][[1]]$label
      pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
      if (v1 > nBSandDepot) { 
        #The last stop was a Boat
        srcID <- sprintf("%s%d","B",(v1 - (nDT + 1))) 
      }else {
        #The last stop was via Gray Depot
        srcID <- sprintf("%s%d","GD",v1) 
      }
      text2print <- paste(text2print,"Shortest rescue path without red edge from",
                          srcID,"to BS",
                          "len =",pLen,":", pStr,"\n")
      #DEBUG
      #cat("***>",text2printCW)
    }#else no red edge to BS
  }#else for check if we already reached BS
  #########################
  #END sp analysis
  #########################
  
  
  
  #text2print <- paste(text2print,"---> Perm",boatPerm,":",totPathLen,"hops - ",round(totPathCost,nPrecDigit),"m\n")
  # my_text <- paste0(my_text, text2print)
  # output$msg1<- renderText({ my_text })
  #DEBUG
  #cat("**********************************************************\n")
  #cat(text2print)
  
  
  
  path2print <- ""
  for (part in (1:PermPathListLen)) {
    path2print <- paste0(path2print, "--", paste(V(gInput)$label[c(PermPathList[[part]])], collapse = "->") )
  }
  
  
  cat("************** The final path:",path2print,"\n\n")
  
  
  
  ############################################
  #BEGIN Calculate AWD for boats
  ###########################################
  #Get the path as a vector
  #thePath <- unlist(PermPathList[[1]]$vpath)
  thePath <- V(gInput)$name[c(PermPathList[[1]])]
  for (p in 2:PermPathListLen) {
    ll <- length(PermPathList[[p]])
    thePath <- c(thePath, V(gInput)$name[c(PermPathList[[p]][2:ll])])
  }
  
  #DEBUG
  cat("Perm path(vtx ids):", thePath,"\n")
  cat("Perm path(labels):", path2print,"\n")
  
  theLen <- length(thePath)
  sumWD <- 0
  nWD <- 0
  nCharging <- 0
  for (p in 1:theLen) {
    if (thePath[p] > nBSandDepot) {
      #we got boat on the path
      
      theDistfromBS <- sum(E(gInput, path = unlist(thePath[1:p]))$weight)
      sumWD <- sumWD + theDistfromBS
      nWD <- nWD + 1
      #DEBUG
      cat(nWD,"--- The dist to Boat:", thePath[p] - nBSandDepot,"is",theDistfromBS,"waiting dist:",
          round((theDistfromBS / currentDroneRange),nPrecDigit),"times the drone range.\n")
    }else{
      if (thePath[p] != 1) {
        #Count the chargings other than BS
        nCharging <- nCharging + 1
      }
    }
  }
  AWD <- sumWD / nWD
  
  #DEBUG
  cat("AWD for", nBT, "boats is", round(AWD,nPrecDigit), "times the drone range.\n")
  cat(nCharging, "full chargings.\n")
  ############################################
  #END Calculate AWD for boats
  ###########################################
  
  
  
  
  
  
  
  ###########################################
  #Save the rescue path and info for this permutation
  ###########################################
  # AllPermPathListLen <- AllPermPathListLen + 1
  # AllPermPathList[[AllPermPathListLen]] <- PermPathList
  # AllPermPathCostList[[AllPermPathListLen]]  <- totPathCost
  # AllPermAWDList[[AllPermPathListLen]]  <-  AWD
  # AllPermNChargingsList[[AllPermPathListLen]]  <- nCharging
  # AllPermPathHopsList[[AllPermPathListLen]] <- totPathLen
  ###########################################
  
  
  
  
  
  
  #Here is the selection of the "optimum" path
  #Consider tie-break rules by adding these measures:
  # AWD
  # The time for the first rescue
  # 
  # if (totPathCost < optimPathCost) {
  #   optimPathCost <- totPathCost
  #   optimPathLen <- totPathLen
  #   optimPermId <- k
  #   optimPath <- totPath
  #   optimPathId <- AllPermPathListLen
  #   optimRescuePathList <- PermPathList
  #   optimRescuePathListLen <- PermPathListLen
  #   optimPrintOrd <- printOrd
  # }
  
  
  #DEBUG
  
  text2print <- paste0(text2print,"my_get_metrics_permDEBUG:\n","---------------------------------------------\n",
                       "The cost: ",round(totPathCost,nPrecDigit),"m, hops: ", totPathLen,"\n",
                       "AWD: ",round(AWD,nPrecDigit),"m\n",
                       "Number of full chargings: ",nCharging,"\n",
                       "The Path: ", path2print,"\n",
                       "---------------------------------------------\n")
  
  #DEBUG
  cat("\n---------------------------------------------\n",
      "BEGIN --- my_get_metrics_permDEBUG:\n",
      "---------------------------------------------\n",
      text2print,
      "END --- my_get_metrics_permDEBUG:\n",
      "---------------------------------------------\n\n"
    )
  
  #DEBUG
  cat("***********************************END my_get_metrics_permDEBUG**************************************\n")
  
  theResult <- list()
  
  #  theResult[[1]] : Path length in meters
  #  theResult[[2]] : Path length in hops
  #  theResult[[3]] : AVG waiting distance in meters
  #  theResult[[4]] : Number of chargings
  #  theResult[[5]] : The path string for printing
  #  theResult[[6]] : The path for coloring
  #  theResult[[7]] : The path len for coloring
  
  theResult[[1]] <- totPathCost
  theResult[[2]] <- totPathLen
  theResult[[3]] <- AWD
  theResult[[4]] <- nCharging
  theResult[[5]] <- path2print
  theResult[[6]] <- PermPathList
  theResult[[7]] <- PermPathListLen
  
  
  
  return(theResult)
  
  
  
  
  
}
##########################################################################################################






















########################################################################################################
# THIS FUNCTION ASSUMEs THAT EVERYTHING IS CONNECTED!!!
#
# This function returns the cost of the path given the perm as 
# Weights for:
# plw: Path length
# awdw: AWD
# ncw: Number of chargings
##########################################################################################################
my_get_cost_permDEBUG <- function(gx, boatPerm, distw, plw, awdw, ncw, currentDroneRange) {
  
  
  
  #get the vector showing the order of rescue (permutation)
  rescueOrd <- boatPerm + (nDT + nBS)
  
  printOrd <- sprintf("%s%d","B",(rescueOrd - (nDT + nBS)))
  
  #The following has to be list of list
  #Permutations and
  #For each permutation for each sp (boat to boat)
  PermPathList <- list()
  PermPathListLen <- 0
  
  totPathLen <- 0
  totPathCost <- 0
  totPath <- ""
  
  text2print <- paste("Trying:",paste(printOrd,collapse = " -> "),":\n")
  
  #DEBUG
  cat("***********************************BEGIN my_get_cost_perm**************************************\n")
  
  cat("Perm is:", rescueOrd, "\n")
  cat(text2print)
  
  
  ##########################################################################################################
  #1/3 --- Start from the BS to first boat
  ##########################################################################################################
  v1 <- 1 #BS
  v2 <- rescueOrd[1] #vertex no of the first boat 
  #DEBUG 
  cat("my_get_cost_perm 1/3 --- Trying sp from",v1,"to",v2,"\n")
  rescueSP <-  shortest_paths(gx, from = v1, to = v2, output="both")
  
  
  #DEBUG cat("OK SP\n")
  
  
  #After each sp check if the last edge is red or not
  #if there is a red edge at the end then for sure there is a gray edge around
  #find the gray edges that sum up to drone range with the red edge and
  #try each gray edge dst vertex (src is the currentr boat) 
  #to find sp to the next boat. Pick the min length sp
  
  #########################
  #BEGIN sp analysis
  #########################
  minsp <- Inf
  bestGrayDepotId <- -1
  splen <- length(rescueSP$epath[[1]])
  if (rescueSP$epath[[1]][splen]$color == redEdgeCol) {
    #DEBUG
    cat("my_get_cost_perm 1/3 --- FROM BS: Red edge from",rescueSP$vpath[[1]][splen]$label,"to",v2,"\n")
    #yes we have red edge in sp
    #Find the best gray edge out of the Boat at the end of the sp
    #nGrayEdges <- sum(BT2DTGrayEdge[(v2 - nBSandDepot),])
    GrayEdgeIdx <- which(BT2DTGrayEdge[(v2 - nBSandDepot),] == 1)
    if (nBT == 1) nextVtx <- 1 else nextVtx <- rescueOrd[2]
    for (m in GrayEdgeIdx) {
      if ((v2 - nBSandDepot) < 0) {cat(m,"- my_get_cost_perm - 1/3 ******************************** ERROR: v2:",v2,"not a boat id!\n")}
      if ( (rescueSP$epath[[1]][splen]$weight + BT2DTMtx[(v2 - nBSandDepot), m]) <= currentDroneRange ){
        #Good gray edge lets try to find sp from the dest depot to the next boat
        GrayDepotSP <-  shortest_paths(gx, from = m, to = nextVtx, output="both")
        CostGrayDepotSP <- sum(E(gx, path=unlist(GrayDepotSP$vpath))$weight)
        if ( CostGrayDepotSP < minsp ) {
          minsp <- CostGrayDepotSP
          bestGrayDepotId <- m
          bestGrayDepotSP <- GrayDepotSP
        }
      }
    }#end for
  }# if red edge at the end of sp
  
  #########################
  #END sp analysis
  #########################
  
  
  
  if (bestGrayDepotId == -1) {
    #The sp ends on a boat
    #DEBUG
    cat("my_get_cost_perm 1/3 --- FROM BS: No red edge\n")
    #The last edge of the first sp is not red edge so normal stuff
    PermPathListLen <- PermPathListLen + 1
    #PermPathList[[PermPathListLen]]  <-  rescueSP
    PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
    
    pLen <- length(rescueSP$vpath[1][[1]]) -1
    totPathLen <-  totPathLen + pLen
    totPathCost <- totPathCost + sum(E(gx, path=unlist(rescueSP$vpath))$weight)
    
    pStr <- rescueSP$vpath[1][[1]]
    #DEBUG cat(pStr,"\n")
    #DEBUG 
    cat("my_get_cost_perm 1/3 --- From BS to first Boat:",pStr,"\n")
    
    totPath <- rescueSP$vpath[1][[1]]$label
    pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
    text2print <- paste(text2print,"Shortest rescue path from BS to", 
                        sprintf("%s%d","B",(v2 - (nDT + 1))),
                        "len =",pLen,":", pStr,"\n")
    #DEBUG 
    #cat("***>",text2print)
    
  }else{
    #The sp ends on a depot via gray edge!!!
    #DEBUG
    cat("my_get_cost_perm 1/3 --- FROM BS: Red edge with gray depot\n")
    #Now we got a red edge so the last point is depot not a boat
    #Normal stuff but add the depot to the rescueSP
    
    #Add the depot vertex and the red edge
    #DEBUG
    cat("my_get_cost_perm 1/3 --- FROM BS: Adding vertex", bestGrayDepotId,"as Gray Depot\n")
    rescueSP$vpath[[1]] <- c(rescueSP$vpath[[1]] ,V(gInput)[bestGrayDepotId])
    #DEBUG
    cat("my_get_cost_perm 1/3 --- FROM BS: Adding edge", v2,"--", bestGrayDepotId,"\n")
    rescueSP$epath[[1]] <- c(rescueSP$epath[[1]] ,E(gInput, c(v2, bestGrayDepotId)))
    #DEBUG cat("Add ok\n")
    
    PermPathListLen <- PermPathListLen + 1
    #PermPathList[[PermPathListLen]]  <-  rescueSP
    PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
    
    pLen <- length(rescueSP$vpath[1][[1]]) -1
    totPathLen <-  totPathLen + pLen
    totPathCost <- totPathCost + sum(E(gx, path=unlist(rescueSP$vpath))$weight)
    
    pStr <- rescueSP$vpath[1][[1]]
    #DEBUG cat(pStr,"\n")
    #DEBUG 
    cat("my_get_cost_perm 1/3 --- From BS to first Boat -> Gray Depot:",pStr,"\n")
    
    totPath <- rescueSP$vpath[1][[1]]$label
    pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
    text2print <- paste(text2print,"Shortest rescue path with red edge from BS to", 
                        sprintf("%s%d","B",(v2 - (nDT + 1))),
                        "len =",pLen,":", pStr,"\n")
    #DEBUG 
    #cat("***>",text2print)
    #Now update the src of the next path as the depot we reach with gray edge
    #WATCH OUT V2 is not boat id anymore!!!
    v2 <- bestGrayDepotId
  }#else we got a red edge
  
  ##########################################################################################################
  #2/3 --- from the first boat jump to next till the last one when we have more than 1 boat
  ##########################################################################################################
  if (nBT > 1) {
    
    for (b in 2:nBT){
      v1 <- v2 #If previous path has red-gray pair then this v1 will be depot id not boat id!!! 
      v2 <- rescueOrd[b]
      #DEBUG 
      cat("my_get_cost_perm 2/3 --- Trying sp from",v1,"to",v2,"\n")
      rescueSP <-  shortest_paths(gx, from = v1, to = v2, output="both")
      
      #After each sp check if the last edge is red or not
      #if there is a red edge at the end then for sure there is a gray edge around
      #find the gray edges that sum up to drone range with the red edge and
      #try each gray edge dst vertex (src is the currentr boat) 
      #to find sp to the next boat. Pick the min length sp
      
      #########################
      #BEGIN sp analysis
      #########################
      minsp <- Inf
      bestGrayDepotId <- -1
      splen <- length(rescueSP$epath[[1]])
      if (rescueSP$epath[[1]][splen]$color == redEdgeCol) {
        #DEBUG
        cat("my_get_cost_perm 2/3 --- BETWEEN BOATS: Red edge from",rescueSP$vpath[[1]][splen]$label,"to",v2,"\n")
        #yes we have red edge in sp
        #Find the best gray edge out of the Boat at the end of the sp
        #nGrayEdges <- sum(BT2DTGrayEdge[(v2 - nBSandDepot),])
        GrayEdgeIdx <- which(BT2DTGrayEdge[(v2 - nBSandDepot),] == 1)
        
        #Here we can have BS as the last vertex in the tour as bestGrayDepot
        #In this case if we can reach BS then it is the best one no need to go other depots
        #However if we still have other boats to rescue than we need to go to the min gray depot
        #to save some distance
        
        if ( ((rescueSP$epath[[1]][splen]$weight + BT2DTMtx[(v2 - nBSandDepot), 1]) <= currentDroneRange ) && 
             (v2 == rescueOrd[nBT]) && 
             (is.element(1,GrayEdgeIdx))
        ) {
          cat("my_get_cost_perm 2/3 --- We got BS as the reachable last stop from the last boat",v2,"via gray edge\n")
          #The last boat to be rescued
          #From that boat if we can reach to BS via red or gray just take it
          #On phase 2/3 we can reach BS from gray edge added before
          # if there is one just take it
          GrayDepotSP <-  shortest_paths(gx, from = 1, to = rescueOrd[2], output="both")
          CostGrayDepotSP <- sum(E(gx, path=unlist( GrayDepotSP$vpath))$weight)
          minsp <- CostGrayDepotSP
          bestGrayDepotId <- 1
          bestGrayDepotSP <- GrayDepotSP
          
          
        }else{
          #Now we know that BS is not among grayDepots that can be chosen for the last boat
          for (m in GrayEdgeIdx) {
            if ((v2 - nBSandDepot) < 0) {cat(m,"- my_get_cost_perm - 2/3 ******************************** ERROR: v2:",v2,"not a boat id!\n")}
            if ( (rescueSP$epath[[1]][splen]$weight + BT2DTMtx[(v2 - nBSandDepot), m]) <= currentDroneRange ){
              #Good gray edge lets try to find sp from the dest depot to the next boat
              GrayDepotSP <-  shortest_paths(gx, from = m, to = rescueOrd[2], output="both")
              CostGrayDepotSP <- sum(E(gx, path=unlist( GrayDepotSP$vpath))$weight)
              if ( CostGrayDepotSP < minsp ) {
                minsp <- CostGrayDepotSP
                bestGrayDepotId <- m
                bestGrayDepotSP <- GrayDepotSP
              }
            }#end if test for drone range
          }#end for
        }#end else
      }# if red edge at the end of sp
      #########################
      #END sp analysis
      #########################
      
      
      
      if (bestGrayDepotId == -1) {
        #DEBUG
        cat("my_get_cost_perm 2/3 --- BETWEEN BOATS: No red edge\n")
        #The last edge of the first sp is not red edge so normal stuff
        PermPathListLen <- PermPathListLen + 1
        #PermPathList[[PermPathListLen]]  <-  rescueSP
        PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
        
        pLen <- length(rescueSP$vpath[1][[1]]) -1
        totPathLen <-  totPathLen + pLen
        totPathCost <- totPathCost + sum(E(gx, path=unlist(rescueSP$vpath))$weight)
        
        pStr <- rescueSP$vpath[1][[1]]
        #DEBUG cat(pStr,"\n")
        #DEBUG 
        cat("my_get_cost_perm 2/3 --- From the first Boat to the others:",pStr,"\n")
        totPath <- rescueSP$vpath[1][[1]]$label
        pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
        
        if (v1 > nBSandDepot) { 
          #The last stop was a Boat
          srcID <- sprintf("%s%d","B",(v1 - (nDT + 1))) 
        }else {
          #The last stop was via Gray Depot
          srcID <- sprintf("%s%d","GD",v1) 
        }
        
        if (v2 > nBSandDepot) { 
          #The last stop was a Boat
          dstID <- sprintf("%s%d","B",(v2 - (nDT + 1))) 
        }else {
          #The last stop was via Gray Depot
          dstID <- sprintf("%s%d","GD",v2) 
        }
        
        
        text2print <- paste(text2print,"Shortest rescue path from", 
                            srcID,"to", dstID,"len =",pLen,":", pStr,"\n")
        
        
        #DEBUG 
        #cat("***>",text2print)
      }else{
        #DEBUG
        cat("my_get_cost_perm 2/3 --- BETWEEN BOATS: Red edge\n")
        #Now we got a red edge so the last point is depot not a boat
        #Normal stuff but add the depot to the rescueSP
        
        #Add the depot vertex and the red edge
        #DEBUG
        cat("my_get_cost_perm 2/3 --- Adding vertex", bestGrayDepotId,"\n")
        rescueSP$vpath[[1]] <- c(rescueSP$vpath[[1]] ,V(gInput)[bestGrayDepotId])
        cat("my_get_cost_perm 2/3 --- Adding edge", v2,"--", bestGrayDepotId,"\n")
        rescueSP$epath[[1]] <- c(rescueSP$epath[[1]] ,E(gInput, c(v2, bestGrayDepotId)))
        #DEBUG cat("Add ok\n")
        
        PermPathListLen <- PermPathListLen + 1
        #PermPathList[[PermPathListLen]]  <-  rescueSP
        PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
        
        pLen <- length(rescueSP$vpath[1][[1]]) -1
        totPathLen <-  totPathLen + pLen
        totPathCost <- totPathCost + sum(E(gx, path=unlist(rescueSP$vpath))$weight)
        
        
        pStr <- rescueSP$vpath[1][[1]]
        #DEBUG cat(pStr,"\n")
        #DEBUG 
        cat("my_get_cost_perm 2/3 --- From the first Boat to the others with red edge:",pStr,"\n")
        totPath <- rescueSP$vpath[1][[1]]$label
        pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
        
        if (v1 > nBSandDepot) { 
          #The last stop was a Boat
          srcID <- sprintf("%s%d","B",(v1 - (nDT + 1))) 
        }else {
          #The last stop was via Gray Depot
          srcID <- sprintf("%s%d","GD",v1) 
        }
        
        if (v2 > nBSandDepot) { 
          #The last stop was a Boat
          dstID <- sprintf("%s%d","B",(v2 - (nDT + 1))) 
        }else {
          #The last stop was via Gray Depot
          dstID <- sprintf("%s%d","GD",v2) 
        }
        
        
        text2print <- paste(text2print,"Shortest rescue path with red edge from", 
                            srcID,"to",dstID,"len =",pLen,":", pStr,"\n")
        
        
        #DEBUG 
        #cat("***>",text2print)
        #Now update the src of the next path as the depot we reach with gray edge
        v2 <- bestGrayDepotId
      }#else we got a red edge
    }#for loop over boats
  } #if there are more than 1 boat
  
  ##########################################################################################################
  #3/3 --- from the last boat to BS back again
  ##########################################################################################################
  
  #May be we already reached BS from the previous phase
  if (v2 == 1) {
    #DEBUG 
    cat("my_get_cost_perm 3/3 --- We already finished in 2/3\n")
  }else{
    v1 <- v2 #from the lst boat CAN BE A DEPOT EVEN BS!!!!
    v2 <- 1 #back to BS
    #DEBUG 
    cat("my_get_cost_perm 3/3 --- Trying sp from",v1,"to",v2,"\n")
    rescueSP <-  shortest_paths(gx, from = v1, to = v2, output="both")
    
    
    #After each sp check if the last edge is red or not
    #if there is a red edge at the end then for sure there is a gray edge around
    #find the gray edges that sum up to drone range with the red edge and
    #try each gray edge dst vertex (src is the current boat) 
    #to find sp to the next boat. Pick the min length sp
    
    #########################
    #BEGIN sp analysis
    #########################
    minsp <- Inf
    bestGrayDepotId <- -1
    splen <- length(rescueSP$epath[[1]])
    
    
    #The dest is 1 which is BS
    #So even if we have red edge at the end (This happen if the boat is close to the BS!!!)
    #No problem as long as its length is less than the drone range
    #The drone will charge to full at the BS!!!
    
    
    if (rescueSP$epath[[1]][splen]$color == redEdgeCol) {
      #DEBUG
      cat("my_get_cost_perm 3/3 --- BACK TO BS: Red edge from",rescueSP$vpath[[1]][splen]$label,"to",v2,"\n")
      PermPathListLen <- PermPathListLen + 1
      #PermPathList[[PermPathListLen]]  <-  rescueSP
      PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
      
      pLen <- length(rescueSP$vpath[1][[1]]) -1
      totPathLen <-  totPathLen + pLen
      totPathCost <- totPathCost + sum(E(gx, path=unlist(rescueSP$vpath))$weight)
      
      
      pStr <- rescueSP$vpath[1][[1]]
      #DEBUG cat(pStr,"\n")
      #DEBUG
      cat("my_get_cost_perm 3/3 --- From the last boat to BS back with red edge:",pStr,"\n")
      
      totPath <- rescueSP$vpath[1][[1]]$label
      pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
      if (v1 > nBSandDepot) { 
        #The last stop was a Boat
        srcID <- sprintf("%s%d","B",(v1 - (nDT + 1))) 
      }else {
        #The last stop was via Gray Depot
        srcID <- sprintf("%s%d","GD",v1 - (nDT + 1)) 
      }
      text2print <- paste(text2print,"Shortest rescue path with red edge from",
                          srcID,"to BS",
                          "len =",pLen,":", pStr,"\n")
      #DEBUG
      #cat("***>",text2printCW)
      
      # if red edge at the end of sp
    }else{
      #No red edge to BS
      cat("my_get_cost_perm 3/3 --- BACK TO BS: No red edge from",rescueSP$vpath[[1]][splen]$label,"to",v2,"\n")
      PermPathListLen <- PermPathListLen + 1
      #PermPathList[[PermPathListLen]]  <-  rescueSP
      PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
      
      pLen <- length(rescueSP$vpath[1][[1]]) -1
      totPathLen <-  totPathLen + pLen
      totPathCost <- totPathCost + sum(E(gx, path=unlist(rescueSP$vpath))$weight)
      
      
      pStr <- rescueSP$vpath[1][[1]]
      #DEBUG cat(pStr,"\n")
      #DEBUG
      cat("my_get_cost_perm 3/3 --- From the last boat to BS back without red edge:",pStr,"\n")
      
      totPath <- rescueSP$vpath[1][[1]]$label
      pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
      if (v1 > nBSandDepot) { 
        #The last stop was a Boat
        srcID <- sprintf("%s%d","B",(v1 - (nDT + 1))) 
      }else {
        #The last stop was via Gray Depot
        srcID <- sprintf("%s%d","GD",v1) 
      }
      text2print <- paste(text2print,"Shortest rescue path without red edge from",
                          srcID,"to BS",
                          "len =",pLen,":", pStr,"\n")
      #DEBUG
      #cat("***>",text2printCW)
    }#else no red edge to BS
  }#else for check if we already reached BS
  #########################
  #END sp analysis
  #########################
  
  
  
  text2print <- paste(text2print,"---> Perm",boatPerm,":",totPathLen,"hops - ",round(totPathCost,nPrecDigit),"m\n")
  # my_text <- paste0(my_text, text2print)
  # output$msg1<- renderText({ my_text })
  #DEBUG
  #cat("**********************************************************\n")
  #cat(text2print)
  
  
  
  path2print <- ""
  for (part in (1:PermPathListLen)) {
    path2print <- paste0(path2print, "--", paste(V(gx)[PermPathList[[part]]$vpath[[1]]]$label, collapse = "->") )
  }
  
  
  
  
  
  
  ############################################
  #BEGIN Calculate AWD for boats
  ###########################################
  #Get the path as a vector
  thePath <- unlist(PermPathList[[1]]$vpath)
  for (p in 2:PermPathListLen) {
    ll <- length(unlist(PermPathList[[p]]$vpath))
    thePath <- c(thePath, unlist(PermPathList[[p]]$vpath)[2:ll])
  }
  
  #DEBUG
  cat("Perm path(vtx ids):", thePath,"\n")
  cat("Perm path(labels):", path2print,"\n")
  
  theLen <- length(thePath)
  sumWD <- 0
  nWD <- 0
  nCharging <- 0
  for (p in 1:theLen) {
    if (thePath[p] > nBSandDepot) {
      #we got boat on the path
      
      theDistfromBS <- sum(E(gRedGray, path = unlist(thePath[1:p]))$weight)
      sumWD <- sumWD + theDistfromBS / currentDroneRange
      nWD <- nWD + 1
      #DEBUG
      cat( nWD,"--- The dist to Boat:", thePath[p] - nBSandDepot,"is",theDistfromBS,"waiting dist:",
           round((theDistfromBS / currentDroneRange),nPrecDigit),"times the drone range.\n")
    }else{
      if (thePath[p] != 1) {
        #Count the chargings other than BS
        nCharging <- nCharging + 1
      }
    }
  }
  AWD <- sumWD / nWD
  
  #DEBUG
  cat("AWD for", nBT, "boats is", round(AWD,nPrecDigit), "times the drone range.\n")
  cat(nCharging, "full chargings.\n")
  ############################################
  #END Calculate AWD for boats
  ###########################################
  
  
  
  
  
  
  
  ###########################################
  #Save the rescue path and info for this permutation
  ###########################################
  # AllPermPathListLen <- AllPermPathListLen + 1
  # AllPermPathList[[AllPermPathListLen]] <- PermPathList
  # AllPermPathCostList[[AllPermPathListLen]]  <- totPathCost
  # AllPermAWDList[[AllPermPathListLen]]  <-  AWD
  # AllPermNChargingsList[[AllPermPathListLen]]  <- nCharging
  # AllPermPathHopsList[[AllPermPathListLen]] <- totPathLen
  ###########################################
  
  
  
  
  
  
  #Here is the selection of the "optimum" path
  #Consider tie-break rules by adding these measures:
  # AWD
  # The time for the first rescue
  # 
  # if (totPathCost < optimPathCost) {
  #   optimPathCost <- totPathCost
  #   optimPathLen <- totPathLen
  #   optimPermId <- k
  #   optimPath <- totPath
  #   optimPathId <- AllPermPathListLen
  #   optimRescuePathList <- PermPathList
  #   optimRescuePathListLen <- PermPathListLen
  #   optimPrintOrd <- printOrd
  # }
  
  
  #DEBUG
  
  text2print <- paste0(text2print,
                       "The cost: ",round(totPathCost,nPrecDigit),"m, hops: ", totPathLen,"\n",
                       "AWD: ",round(AWD,nPrecDigit),"\n",
                       "Number of full chargings: ",nCharging,"\n",
                       "The Path: ", path2print,"\n",
                       "---------------------------------------------\n")
  
  
  cat(text2print)
  
  #DEBUG
  cat("***********************************END my_get_cost_perm**************************************\n")
  theCost <- distw * totPathCost  + plw * totPathLen + awdw * AWD + ncw * nCharging
  
  return(theCost)
  
  
  
  
  
}
##########################################################################################################





########################################################################################################
# THIS FUNCTION ASSUMEs THAT EVERYTHING IS CONNECTED!!!
# NEEDS OPTIMIZATION SINCE THERE IS NO OUTPUT MSGS
# This function returns the cost of the path given the boat perm as vector from 1..nBT
# Weights for:
# plw: Path length
# awdw: AWD
# ncw: Number of chargings
##########################################################################################################
my_get_cost_perm <- function(gx, boatPerm, distw, plw, awdw, ncw, currentDroneRange) {
  
  
  
  #get the vector showing the order of rescue (permutation)
  rescueOrd <- boatPerm + (nDT + nBS)
  
  printOrd <- sprintf("%s%d","B",(rescueOrd - (nDT + nBS)))
  
  #The following has to be list of list
  #Permutations and
  #For each permutation for each sp (boat to boat)
  PermPathList <- list()
  PermPathListLen <- 0
  
  totPathLen <- 0
  totPathCost <- 0
  totPath <- ""
  
  
  
  #DEBUG
  #text2print <- paste("Trying:",paste(printOrd,collapse = " -> "),":\n")
  #cat("***********************************BEGIN my_get_cost_perm**************************************\n")
  
  #cat("Perm is:", rescueOrd, "\n")
  #cat(text2print)
  
  
  ##########################################################################################################
  #1/3 --- Start from the BS to first boat
  ##########################################################################################################
  v1 <- 1 #BS
  v2 <- rescueOrd[1] #vertex no of the first boat 
  #DEBUG 
  #cat("my_get_cost_perm 1/3 --- Trying sp from",v1,"to",v2,"\n")
  rescueSP <-  shortest_paths(gx, from = v1, to = v2, output="both")
  #DEBUG cat("OK SP\n")
  
  
  #After each sp check if the last edge is red or not
  #if there is a red edge at the end then for sure there is a gray edge around
  #find the gray edges that sum up to drone range with the red edge and
  #try each gray edge dst vertex (src is the currentr boat) 
  #to find sp to the next boat. Pick the min length sp
  
  #########################
  #BEGIN sp analysis
  #########################
  minsp <- Inf
  bestGrayDepotId <- -1
  splen <- length(rescueSP$epath[[1]])
  if (rescueSP$epath[[1]][splen]$color == redEdgeCol) {
    #DEBUG
    #cat("my_get_cost_perm 1/3 --- FROM BS: Red edge from",rescueSP$vpath[[1]][splen]$label,"to",v2,"\n")
    #yes we have red edge in sp
    #Find the best gray edge out of the Boat at the end of the sp
    #nGrayEdges <- sum(BT2DTGrayEdge[(v2 - nBSandDepot),])
    GrayEdgeIdx <- which(BT2DTGrayEdge[(v2 - nBSandDepot),] == 1)
    if (nBT == 1) nextVtx <- 1 else nextVtx <- rescueOrd[2]
    for (m in GrayEdgeIdx) {
      if ((v2 - nBSandDepot) < 0) {cat(m,"- my_get_cost_perm - 1/3 ******************************** ERROR: v2:",v2,"not a boat id!\n")}
      if ( (rescueSP$epath[[1]][splen]$weight + BT2DTMtx[(v2 - nBSandDepot), m]) <= currentDroneRange ){
        #Good gray edge lets try to find sp from the dest depot to the next boat
        GrayDepotSP <-  shortest_paths(gx, from = m, to = nextVtx, output="both")
        CostGrayDepotSP <- sum(E(gx, path=unlist( GrayDepotSP$vpath))$weight)
        if ( CostGrayDepotSP < minsp ) {
          minsp <- CostGrayDepotSP
          bestGrayDepotId <- m
          bestGrayDepotSP <- GrayDepotSP
        }
      }
    }#end for
  }# if red edge at the end of sp
  
  #########################
  #END sp analysis
  #########################
  
  
  
  if (bestGrayDepotId == -1) {
    #The sp ends on a boat
    #DEBUG
    #cat("my_get_cost_perm 1/3 --- FROM BS: No red edge\n")
    #The last edge of the first sp is not red edge so normal stuff
    PermPathListLen <- PermPathListLen + 1
    #PermPathList[[PermPathListLen]]  <-  rescueSP
    PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
    
    pLen <- length(rescueSP$vpath[1][[1]]) -1
    totPathLen <-  totPathLen + pLen
    totPathCost <- totPathCost + sum(E(gx, path=unlist(rescueSP$vpath))$weight)
    
    pStr <- rescueSP$vpath[1][[1]]
    #DEBUG cat(pStr,"\n")
    #DEBUG 
    #cat("my_get_cost_perm 1/3 --- From BS to first Boat:",pStr,"\n")
    
    totPath <- rescueSP$vpath[1][[1]]$label
    pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
    #text2print <- paste(text2print,"Shortest rescue path from BS to", 
    # sprintf("%s%d","B",(v2 - (nDT + 1))),
    # "len =",pLen,":", pStr,"\n")
    #DEBUG 
    #cat("***>",text2print)
    
  }else{
    #The sp ends on a depot via gray edge!!!
    #DEBUG
    #cat("my_get_cost_perm 1/3 --- FROM BS: Red edge with gray depot\n")
    #Now we got a red edge so the last point is depot not a boat
    #Normal stuff but add the depot to the rescueSP
    
    #Add the depot vertex and the red edge
    #DEBUG
    #cat("my_get_cost_perm 1/3 --- FROM BS: Adding vertex", bestGrayDepotId,"as Gray Depot\n")
    rescueSP$vpath[[1]] <- c(rescueSP$vpath[[1]] ,V(gInput)[bestGrayDepotId])
    #DEBUG
    #cat("my_get_cost_perm 1/3 --- FROM BS: Adding edge", v2,"--", bestGrayDepotId,"\n")
    rescueSP$epath[[1]] <- c(rescueSP$epath[[1]] ,E(gInput, c(v2, bestGrayDepotId)))
    #DEBUG cat("Add ok\n")
    
    PermPathListLen <- PermPathListLen + 1
    #PermPathList[[PermPathListLen]]  <-  rescueSP
    PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
    
    pLen <- length(rescueSP$vpath[1][[1]]) -1
    totPathLen <-  totPathLen + pLen
    totPathCost <- totPathCost + sum(E(gx, path=unlist(rescueSP$vpath))$weight)
    
    pStr <- rescueSP$vpath[1][[1]]
    #DEBUG cat(pStr,"\n")
    #DEBUG 
    #cat("my_get_cost_perm 1/3 --- From BS to first Boat -> Gray Depot:",pStr,"\n")
    
    totPath <- rescueSP$vpath[1][[1]]$label
    pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
    #text2print <- paste(text2print,"Shortest rescue path with red edge from BS to", 
    # sprintf("%s%d","B",(v2 - (nDT + 1))),
    # "len =",pLen,":", pStr,"\n")
    #DEBUG 
    #cat("***>",text2print)
    #Now update the src of the next path as the depot we reach with gray edge
    #WATCH OUT V2 is not boat id anymore!!!
    v2 <- bestGrayDepotId
  }#else we got a red edge
  
  ##########################################################################################################
  #2/3 --- from the first boat jump to next till the last one when we have more than 1 boat
  ##########################################################################################################
  if (nBT > 1) {
    
    for (b in 2:nBT){
      v1 <- v2 #If previous path has red-gray pair then this v1 will be depot id not boat id!!! 
      v2 <- rescueOrd[b]
      #DEBUG 
      #cat("my_get_cost_perm 2/3 --- Trying sp from",v1,"to",v2,"\n")
      rescueSP <-  shortest_paths(gx, from = v1, to = v2, output="both")
      
      #After each sp check if the last edge is red or not
      #if there is a red edge at the end then for sure there is a gray edge around
      #find the gray edges that sum up to drone range with the red edge and
      #try each gray edge dst vertex (src is the currentr boat) 
      #to find sp to the next boat. Pick the min length sp
      
      #########################
      #BEGIN sp analysis
      #########################
      minsp <- Inf
      bestGrayDepotId <- -1
      splen <- length(rescueSP$epath[[1]])
      if (rescueSP$epath[[1]][splen]$color == redEdgeCol) {
        #DEBUG
        #cat("my_get_cost_perm 2/3 --- BETWEEN BOATS: Red edge from",rescueSP$vpath[[1]][splen]$label,"to",v2,"\n")
        #yes we have red edge in sp
        #Find the best gray edge out of the Boat at the end of the sp
        #nGrayEdges <- sum(BT2DTGrayEdge[(v2 - nBSandDepot),])
        GrayEdgeIdx <- which(BT2DTGrayEdge[(v2 - nBSandDepot),] == 1)
        
        #Here we can have BS as the last vertex in the tour as bestGrayDepot
        #In this case if we can reach BS then it is the best one no need to go other depots
        #However if we still have other boats to rescue than we need to go to the min gray depot
        #to save some distance
        
        if ( ((rescueSP$epath[[1]][splen]$weight + BT2DTMtx[(v2 - nBSandDepot), 1]) <= currentDroneRange ) && 
             (v2 == rescueOrd[nBT]) && 
             (is.element(1,GrayEdgeIdx))
        ) {
          #cat("my_get_cost_perm 2/3 --- We got BS as the reachable last stop from the last boat",v2,"via gray edge\n")
          #The last boat to be rescued
          #From that boat if we can reach to BS via red or gray just take it
          #On phase 2/3 we can reach BS from gray edge added before
          # if there is one just take it
          GrayDepotSP <-  shortest_paths(gx, from = 1, to = rescueOrd[2], output="both")
          CostGrayDepotSP <- sum(E(gx, path=unlist( GrayDepotSP$vpath))$weight)
          minsp <- CostGrayDepotSP
          bestGrayDepotId <- 1
          bestGrayDepotSP <- GrayDepotSP
          
          
        }else{
          #Now we know that BS is not among grayDepots that can be chosen for the last boat
          for (m in GrayEdgeIdx) {
            if ((v2 - nBSandDepot) < 0) {cat(m,"- my_get_cost_perm - 2/3 ******************************** ERROR: v2:",v2,"not a boat id!\n")}
            if ( (rescueSP$epath[[1]][splen]$weight + BT2DTMtx[(v2 - nBSandDepot), m]) <= currentDroneRange ){
              #Good gray edge lets try to find sp from the dest depot to the next boat
              GrayDepotSP <-  shortest_paths(gx, from = m, to = rescueOrd[2], output="both")
              CostGrayDepotSP <- sum(E(gx, path=unlist( GrayDepotSP$vpath))$weight)
              if ( CostGrayDepotSP < minsp ) {
                minsp <- CostGrayDepotSP
                bestGrayDepotId <- m
                bestGrayDepotSP <- GrayDepotSP
              }
            }#end if test for drone range
          }#end for
        }#end else
      }# if red edge at the end of sp
      #########################
      #END sp analysis
      #########################
      
      
      
      if (bestGrayDepotId == -1) {
        #DEBUG
        #cat("my_get_cost_perm 2/3 --- BETWEEN BOATS: No red edge\n")
        #The last edge of the first sp is not red edge so normal stuff
        PermPathListLen <- PermPathListLen + 1
        #PermPathList[[PermPathListLen]]  <-  rescueSP
        PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
        
        pLen <- length(rescueSP$vpath[1][[1]]) -1
        totPathLen <-  totPathLen + pLen
        totPathCost <- totPathCost + sum(E(gx, path=unlist(rescueSP$vpath))$weight)
        
        pStr <- rescueSP$vpath[1][[1]]
        #DEBUG cat(pStr,"\n")
        #DEBUG 
        #cat("my_get_cost_perm 2/3 --- From the first Boat to the others:",pStr,"\n")
        totPath <- rescueSP$vpath[1][[1]]$label
        pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
        
        if (v1 > nBSandDepot) { 
          #The last stop was a Boat
          srcID <- sprintf("%s%d","B",(v1 - (nDT + 1))) 
        }else {
          #The last stop was via Gray Depot
          srcID <- sprintf("%s%d","GD",v1) 
        }
        
        if (v2 > nBSandDepot) { 
          #The last stop was a Boat
          dstID <- sprintf("%s%d","B",(v2 - (nDT + 1))) 
        }else {
          #The last stop was via Gray Depot
          dstID <- sprintf("%s%d","GD",v2) 
        }
        
        
        #text2print <- paste(text2print,"Shortest rescue path from", 
        #srcID,"to", dstID,"len =",pLen,":", pStr,"\n")
        
        
        #DEBUG 
        #cat("***>",text2print)
      }else{
        #DEBUG
        #cat("my_get_cost_perm 2/3 --- BETWEEN BOATS: Red edge\n")
        #Now we got a red edge so the last point is depot not a boat
        #Normal stuff but add the depot to the rescueSP
        
        #Add the depot vertex and the red edge
        #DEBUG
        #cat("my_get_cost_perm 2/3 --- Adding vertex", bestGrayDepotId,"\n")
        rescueSP$vpath[[1]] <- c(rescueSP$vpath[[1]] ,V(gInput)[bestGrayDepotId])
        #cat("my_get_cost_perm 2/3 --- Adding edge", v2,"--", bestGrayDepotId,"\n")
        rescueSP$epath[[1]] <- c(rescueSP$epath[[1]] ,E(gInput, c(v2, bestGrayDepotId)))
        #DEBUG cat("Add ok\n")
        
        PermPathListLen <- PermPathListLen + 1
        #PermPathList[[PermPathListLen]]  <-  rescueSP
        PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
        
        pLen <- length(rescueSP$vpath[1][[1]]) -1
        totPathLen <-  totPathLen + pLen
        totPathCost <- totPathCost + sum(E(gx, path=unlist(rescueSP$vpath))$weight)
        
        
        pStr <- rescueSP$vpath[1][[1]]
        #DEBUG cat(pStr,"\n")
        #DEBUG 
        #cat("my_get_cost_perm 2/3 --- From the first Boat to the others with red edge:",pStr,"\n")
        totPath <- rescueSP$vpath[1][[1]]$label
        pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
        
        if (v1 > nBSandDepot) { 
          #The last stop was a Boat
          srcID <- sprintf("%s%d","B",(v1 - (nDT + 1))) 
        }else {
          #The last stop was via Gray Depot
          srcID <- sprintf("%s%d","GD",v1) 
        }
        
        if (v2 > nBSandDepot) { 
          #The last stop was a Boat
          dstID <- sprintf("%s%d","B",(v2 - (nDT + 1))) 
        }else {
          #The last stop was via Gray Depot
          dstID <- sprintf("%s%d","GD",v2) 
        }
        
        
        # text2print <- paste(text2print,"Shortest rescue path with red edge from", 
        #                     srcID,"to",dstID,"len =",pLen,":", pStr,"\n")
        
        
        #DEBUG 
        #cat("***>",text2print)
        #Now update the src of the next path as the depot we reach with gray edge
        v2 <- bestGrayDepotId
      }#else we got a red edge
    }#for loop over boats
  } #if there are more than 1 boat
  
  ##########################################################################################################
  #3/3 --- from the last boat to BS back again
  ##########################################################################################################
  
  #May be we already reached BS from the previous phase
  if (v2 == 1) {
    #DEBUG 
    #cat("my_get_cost_perm 3/3 --- We already finished in 2/3\n")
  }else{
    v1 <- v2 #from the lst boat CAN BE A DEPOT EVEN BS!!!!
    v2 <- 1 #back to BS
    #DEBUG 
    #cat("my_get_cost_perm 3/3 --- Trying sp from",v1,"to",v2,"\n")
    rescueSP <-  shortest_paths(gx, from = v1, to = v2, output="both")
    
    
    #After each sp check if the last edge is red or not
    #if there is a red edge at the end then for sure there is a gray edge around
    #find the gray edges that sum up to drone range with the red edge and
    #try each gray edge dst vertex (src is the current boat) 
    #to find sp to the next boat. Pick the min length sp
    
    #########################
    #BEGIN sp analysis
    #########################
    minsp <- Inf
    bestGrayDepotId <- -1
    splen <- length(rescueSP$epath[[1]])
    
    
    #The dest is 1 which is BS
    #So even if we have red edge at the end (This happen if the boat is close to the BS!!!)
    #No problem as long as its length is less than the drone range
    #The drone will charge to full at the BS!!!
    
    
    if (rescueSP$epath[[1]][splen]$color == redEdgeCol) {
      #DEBUG
      #cat("my_get_cost_perm 3/3 --- BACK TO BS: Red edge from",rescueSP$vpath[[1]][splen]$label,"to",v2,"\n")
      PermPathListLen <- PermPathListLen + 1
      #PermPathList[[PermPathListLen]]  <-  rescueSP
      PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
      
      pLen <- length(rescueSP$vpath[1][[1]]) -1
      totPathLen <-  totPathLen + pLen
      totPathCost <- totPathCost + sum(E(gx, path=unlist(rescueSP$vpath))$weight)
      
      
      pStr <- rescueSP$vpath[1][[1]]
      #DEBUG cat(pStr,"\n")
      #DEBUG
      #cat("my_get_cost_perm 3/3 --- From the last boat to BS back with red edge:",pStr,"\n")
      
      totPath <- rescueSP$vpath[1][[1]]$label
      pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
      if (v1 > nBSandDepot) { 
        #The last stop was a Boat
        srcID <- sprintf("%s%d","B",(v1 - (nDT + 1))) 
      }else {
        #The last stop was via Gray Depot
        srcID <- sprintf("%s%d","GD",v1 - (nDT + 1)) 
      }
      # text2print <- paste(text2print,"Shortest rescue path with red edge from",
      #                     srcID,"to BS",
      #                     "len =",pLen,":", pStr,"\n")
      #DEBUG
      #cat("***>",text2printCW)
      
      # if red edge at the end of sp
    }else{
      #No red edge to BS
      #cat("my_get_cost_perm 3/3 --- BACK TO BS: No red edge from",rescueSP$vpath[[1]][splen]$label,"to",v2,"\n")
      PermPathListLen <- PermPathListLen + 1
      #PermPathList[[PermPathListLen]]  <-  rescueSP
      PermPathList[[PermPathListLen]]  <-  unlist(rescueSP$vpath[[1]])$name
      
      pLen <- length(rescueSP$vpath[1][[1]]) -1
      totPathLen <-  totPathLen + pLen
      totPathCost <- totPathCost + sum(E(gx, path=unlist(rescueSP$vpath))$weight)
      
      
      pStr <- rescueSP$vpath[1][[1]]
      #DEBUG cat(pStr,"\n")
      #DEBUG
      #cat("my_get_cost_perm 3/3 --- From the last boat to BS back without red edge:",pStr,"\n")
      
      totPath <- rescueSP$vpath[1][[1]]$label
      pStr <- paste(as.character(paste(rescueSP$vpath[1][[1]]$label)), collapse = " -> ")
      if (v1 > nBSandDepot) { 
        #The last stop was a Boat
        srcID <- sprintf("%s%d","B",(v1 - (nDT + 1))) 
      }else {
        #The last stop was via Gray Depot
        srcID <- sprintf("%s%d","GD",v1) 
      }
      #text2print <- paste(text2print,"Shortest rescue path without red edge from",
      #                   srcID,"to BS","len =",pLen,":", pStr,"\n")
      #DEBUG
      #cat("***>",text2printCW)
    }#else no red edge to BS
  }#else for check if we already reached BS
  #########################
  #END sp analysis
  #########################
  
  
  
  #text2print <- paste(text2print,"---> Perm",boatPerm,":",totPathLen,"hops - ",round(totPathCost,nPrecDigit),"m\n")
  # my_text <- paste0(my_text, text2print)
  # output$msg1<- renderText({ my_text })
  #DEBUG
  #cat("**********************************************************\n")
  #cat(text2print)
  
  
  
  #path2print <- ""
  #for (part in (1:PermPathListLen)) {
  #  path2print <- paste0(path2print, "--", paste(V(gx)[PermPathList[[part]]$vpath[[1]]]$label, collapse = "->") )
  #}
  
  
  
  
  
  
  ############################################
  #BEGIN Calculate AWD for boats
  ###########################################
  #Get the path as a vector
  thePath <- unlist(PermPathList[[1]]$vpath)
  for (p in 2:PermPathListLen) {
    ll <- length(unlist(PermPathList[[p]]$vpath))
    thePath <- c(thePath, unlist(PermPathList[[p]]$vpath)[2:ll])
  }
  
  #DEBUG
  #cat("Perm path(vtx ids):", thePath,"\n")
  #cat("Perm path(labels):", path2print,"\n")
  
  theLen <- length(thePath)
  sumWD <- 0
  nWD <- 0
  nCharging <- 0
  for (p in 1:theLen) {
    if (thePath[p] > nBSandDepot) {
      #we got boat on the path
      
      theDistfromBS <- sum(E(gRedGray, path = unlist(thePath[1:p]))$weight)
      sumWD <- sumWD + theDistfromBS / currentDroneRange
      nWD <- nWD + 1
      #DEBUG
      # cat( nWD,"--- The dist to Boat:", thePath[p] - nBSandDepot,"is",theDistfromBS,"waiting dist:",
      #      round((theDistfromBS / currentDroneRange),nPrecDigit),"times the drone range.\n")
    }else{
      if (thePath[p] != 1) {
        #Count the chargings other than BS
        nCharging <- nCharging + 1
      }
    }
  }
  AWD <- sumWD / nWD
  
  #DEBUG
  #cat("AWD for", nBT, "boats is", round(AWD,nPrecDigit), "times the drone range.\n")
  #cat(nCharging, "full chargings.\n")
  ############################################
  #END Calculate AWD for boats
  ###########################################
  
  
  
  
  
  
  
  ###########################################
  #Save the rescue path and info for this permutation
  ###########################################
  # AllPermPathListLen <- AllPermPathListLen + 1
  # AllPermPathList[[AllPermPathListLen]] <- PermPathList
  # AllPermPathCostList[[AllPermPathListLen]]  <- totPathCost
  # AllPermAWDList[[AllPermPathListLen]]  <-  AWD
  # AllPermNChargingsList[[AllPermPathListLen]]  <- nCharging
  # AllPermPathHopsList[[AllPermPathListLen]] <- totPathLen
  ###########################################
  
  
  
  
  
  
  #Here is the selection of the "optimum" path
  #Consider tie-break rules by adding these measures:
  # AWD
  # The time for the first rescue
  # 
  # if (totPathCost < optimPathCost) {
  #   optimPathCost <- totPathCost
  #   optimPathLen <- totPathLen
  #   optimPermId <- k
  #   optimPath <- totPath
  #   optimPathId <- AllPermPathListLen
  #   optimRescuePathList <- PermPathList
  #   optimRescuePathListLen <- PermPathListLen
  #   optimPrintOrd <- printOrd
  # }
  
  
  #DEBUG
  
  # text2print <- paste0(text2print,
  #                      "The cost: ",round(totPathCost,nPrecDigit),"m, hops: ", totPathLen,"\n",
  #                      "AWD: ",round(AWD,nPrecDigit),"\n",
  #                      "Number of full chargings: ",nCharging,"\n",
  #                      "The Path: ", path2print,"\n",
  #                      "---------------------------------------------\n")
  
  
  #cat(text2print)
  
  #DEBUG
  #cat("***********************************END my_get_cost_perm**************************************\n")
  theCost <- distw * totPathCost +  plw * totPathLen + awdw * AWD + ncw * nCharging
  
  return(-theCost)
  
  
  
  
  
}
##########################################################################################################





########################################################################################################
# End Functions
########################################################################################################

