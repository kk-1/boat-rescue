
########################################################################################################
# Define UI for app
########################################################################################################
ui <- fluidPage(
  
  #Change the font size for paper images!!!
  #Comment these lines for the user demo run
  tags$style(type = "text/css",
             "label { font-size: 20px; }",
             "select { font-size: 20px; }",
             "input { font-size: 20px; }",
             "#startPoly{  font-size: 20px;}",
             "#startBS{  font-size: 20px;}",
             "#clearSelectionRegion{  font-size: 20px;}",
             "#closePolygon{  font-size: 20px;}",
             "#deployDepots{  font-size: 20px;}",
             "#rescueBoats{  font-size: 20px;}",
             "#DroneNumber{  font-size: 20px;}",
             "#cov{  font-size: 20px;}",
             "#myMsg{  font-size: 20px;}",
             "#clickMsg{  font-size: 20px;}",
             "#mouseCoord{  font-size: 20px;}",
             "#saveMap{  font-size: 20px;}",
             "#dl{  font-size: 20px;}",
  ),

  
  useShinyjs(),
  # App title ----
  titlePanel("BoatRescueSim - V81"),
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      div(style="display: inline-block;vertical-align:top;",
          switchInput(inputId = "startPoly", label = "Select Region Pts",  value = FALSE)),
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
      
      div(style="display: inline-block;vertical-align:top;",
          switchInput(inputId = "startBS", label = "Select BS Pos",  value = FALSE)), 
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
      
      div(style="display: inline-block;vertical-align:top;",
          switchInput(inputId = "startBT", label = "Select Boat Pos",  value = FALSE)), 
      
      hr(style = "border-top: 1px solid #000000;"),
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("clearSelectionRegion", label = "Reset")),
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("closePolygon", label = "ClosePoly")), 
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("deployTriDepots", label = "TriGrid")), 
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("deploySqDepots", label = "SqGrid")), 
      
      
      
      hr(style = "border-top: 1px solid #000000;"),
      
      div(style="display: inline-block;vertical-align:top;",
          radioButtons(inputId = "grid", label = "Grid type:",
                       c("Triangular" = "tri", 
                         "Square" = "sq"),
                       selected ="tri" )),
      #hr(),
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
      
      div(style="display: inline-block;vertical-align:top;",
          radioButtons("edgeFilter", "Edge Filter:",
                       c("Gray" = "gr", 
                         "Gray+Red" = "rg",  
                         "Gray+Red+Yellow" = "rgy"),
                       selected ="rg" )),
      hr(style = "border-top: 1px solid #000000;"),
      
      # Input: Slider for drone range ----
      sliderInput(inputId = "rnd_boat_number",
                  label = "RND Boat Number:",
                  min = 1,
                  max = maxBoats,
                  step = 5,
                  value = 100
      ), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("rndBoats", label = "RND Boats")),
      
      
      hr(style = "border-top: 1px solid #000000;"),
      
      
      # Input: Slider for drone range ----
      sliderInput(inputId = "drone_range",
                  label = "Drone Range:",
                  min = min_drone_range,
                  max = max_drone_range,
                  step = 500,
                  value = floor((max_drone_range + min_drone_range)/2)
      ), 
      hr(),
      
      
     
      
      hr(style = "border-top: 1px solid #000000;"),
      
      # Input: Slider for sims ----
      sliderInput(inputId = "nSim",
                  label = "N Sim:",
                  min = 1,
                  max = 100,
                  step = 1,
                  value = 20
      ), 
      div(style="display: inline-block;vertical-align:top;",
          actionButton("runSim", label = "RunSim")), 
      
      
      hr(style = "border-top: 1px solid #000000;"),
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("concaveTSPRedGray", label = "concaveTSP-RG")),
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("concaveTSPGray", label = "concaveTSP-G")), 
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("concaveTSPRedGrayYellow", label = "concaveTSP-RGY")), 
      
  
      
      hr(),
      div(style="display: inline-block;vertical-align:top;",
          actionButton("xTSPRedGray", label = "xTSP-RG")), 
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("xTSPGray", label = "xTSP-G")), 
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("xTSPRedGrayYellow", label = "xTSP-RGY")),
      
      selectInput("tspAlgo", 
                  "Select TSP", 
                  choices = c("nearest_insertion",
                              "farthest_insertion",
                              "cheapest_insertion",
                              "arbitrary_insertion",
                              "nn",
                              "repetitive_nn",
                              "two_opt"),
                  selected = "nn"),
      
      
      
      
      
      
      
      hr(),
      
      
      
      
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("rescueRedGray1by1", label = "Rescue1by1-RG")),  
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("rescueGray1by1", label = "Rescue1by1-G")), 
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("rescueRedGrayYellow1by1", label = "Rescue1by1-RGY")), 
      
      
      hr(),
      div(style="display: inline-block;vertical-align:top;",
          actionButton("permRedGray", label = "bruteForce-RG")), 
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("permGray", label = "bruteForce-G")), 
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("permRedGrayYellow", label = "bruteForce-RGY")),
      
      
     
      
      
      
      hr(),
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("clookRedGray", label = "clookAll-RG")),
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("clookGray", label = "clookAll-G")), 
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("clookRedGrayYellow", label = "clookAll-RGY")), 
      
      
      hr(),
      
     
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("gaPermRedGray", label = "GAperm-RG")),
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("gaSectorRedGray", label = "GAsector-G")), 
      
      
      hr(),
      
      div(style="display: inline-block;vertical-align:top;",
          actionButton("saveMap", label = "save-map")),
      
      
      
      hr(style = "border-top: 1px solid #000000;"),
      
      # 
      # div(style="display: inline-block;vertical-align:top;",
      #     actionButton("hexGrid", label = "HexGrid")), 
      # 
      # div(style="display: inline-block;vertical-align:top;",
      #     actionButton("rGrid", label = "SqGrid")), 
      
      
      
     
      hr(),
      
      
      # Input: Slider for depot spacing ----
      #display dynamic UI
      uiOutput("depot_spacing"),
      hr(style = "border-top: 1px solid #000000;"),
      # # Input: Slider for scaling factor graph plot ----
      # sliderInput(inputId = "myGraphScale",
      #             label = "Graph Scale:",
      #             min = 10,
      #             max = 20,
      #             step = 1,
      #             value = 15
      # ), hr(),
      
      
      
      
      #To show the coverage of the depots
      switchInput(inputId = "cov", label = "Coverage",  value = FALSE), 
      hr(style = "border-top: 1px solid #000000;"),
      
      
      
      htmlOutput("info"), hr(),
      verbatimTextOutput("msg1"), 
      hr(style = "border-top: 1px solid #000000;"),
      
      
      #Mouse stuff for debugging
      h5("Click msg:"),
      verbatimTextOutput("clickMsg",placeholder = TRUE), hr(),
      h5("Mouse coord:"),
      verbatimTextOutput("mouseCoord",placeholder = TRUE),
      
      
    ),
    
    
    
    
    
    mainPanel(
      leafletOutput(outputId = "mymap",width="1200", height="550"),
      #hr(),
      #titlePanel("Geographical"),leafletOutput(outputId = "mymap", width="1000px", height="650px"),
      #leafletOutput(outputId = "mymap", width="1000px", height="540px"),
      hr(),
      #titlePanel("Graph"), svgPanZoomOutput(outputId = "iPlot", width="1500px", height="800px"),
      svgPanZoomOutput(outputId = "iPlot", width="1000px", height="540px"),
    )
    
  )
)






















#   
#   
#   
#   
#   
#   
#   
#   # App title ----
#   titlePanel("Drone Rescue - V1"),
#   
#   fluidRow(
#     
#     # Output: Graph plots in image form and also igraph form ----
#     # column(5,plotOutput(outputId = "distPlot")),
#     # column(5,plotOutput(outputId = "iPlot", width="1000px", height="500px")),
#     column(5, titlePanel("Geographical"), plotOutput(outputId = "distPlot", width="600px", height="400px")),
#     column(5, titlePanel("Graph"), plotOutput(outputId = "iPlot", width="600px", height="400px")),
#   ),
#   
#   hr(),
#   
#   fluidRow(
#     column(3,
#            # Input: Slider for the x for BS ----
#            sliderInput(inputId = "bsx",
#                        label = "BS X:",
#                        min = 1,
#                        max = w,
#                        value = floor(w/2)
#            ),
#            # Input: Slider for y for BS ----
#            sliderInput(inputId = "bsy",
#                        label = "BS Y:",
#                        min = 1,
#                        max = h,
#                        value = floor(h/2)
#            ),
#            
#            # Input: Slider for drone range ----
#            sliderInput(inputId = "drone_range",
#                        label = "Drone Range:",
#                        min = 1,
#                        max = max_drone_range,
#                        value = floor(max_drone_range/2)
#            ),
#            
#            # Input: Slider for depot spacing ----
#            #display dynamic UI
#            uiOutput("depot_spacing")
#     ),
#     column(3,
#            # Input: Slider for the x for Boat----
#            sliderInput(inputId = "nBoat",
#                        label = "Number of Boats:",
#                        min = 1,
#                        max = maxBoats,
#                        value = 1
#                        
#            ),
#            
#            switchInput(inputId = "cov", label = "Coverage",  value = FALSE)
#            #materialSwitch(inputId = "cov", label = "Coverage Switch", status = "ShowCoverage")
#            
#     ),
#     
#     column(4,
#            htmlOutput("info")
#            
#     ),
#     
#     column(5,
#            #actionButton("msgPrint", label = "Show SPs"),
#            #actionButton("msgClear", label = "Clear SPs"),
#            verbatimTextOutput("msg1")
#            
#     ),
#     
#     # column(5, offset = 1, htmlOutput("msg2"))
#   )
#   
# )

########################################################################################################

