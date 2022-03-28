########################################################################################################
########################################################################################################
 #This is script version cp it to the directory with the results-01.csv and run it from there
########################################################################################################
#setwd("/home/kemal/Downloads")
#myWorkDir <- "/my_hd1/my_dir/my_prg/Camerino/boat-rescue/rescueWithMapUI"
#setwd(myWorkDir)
########################################################################################################
#Clean the environment
remove(list = ls())
########################################################################################################

library(tidyverse)
library(ggplot2)
library(reshape2)
library(data.table)
library(dplyr)

####################################################################################
# Statistics variables
####################################################################################
#myDataDir  <- "/my_hd1/my_dir/my_prg/Camerino/boat-rescue/rescueWithMapUI/data"
myResultDir  <- "."

resultFileName <- paste0("results-01.csv")
resultFile <- paste(myResultDir,resultFileName, sep = "/")
cat("Reading benchmark results from", resultFile,"\n")


#Counter for boatpos file
boatPosCNT <- 0

# boatPosTXT <- str_pad(boatPosCNT, 4, pad = "0")
# 
# boatPosFileName <- paste0("boatPos-",boatPosTXT,".csv")
# boatPosFile <- paste(myDataDir,boatPosFileName, sep = "/")
# cat("Saving boat configuration to", boatPosFile,"\n")



#Recreate the results file!!!

#theColNames <- c("GridType", "Algo", "NBoat", "Cost", "AWD", "NCharging", "NCS", "Time", "BtPosFName", "BPerm","SPath")
theColNames <- "GridType, Algo, NBoat, Cost, AWD, NCharging, NCS, Time, BtPosFName, BPerm, SPath"



########################################################################################################
#List the algos for the reference!!!!
allAlgoList <- c("permRedGray", 
                 "permGray",  
                 "clookRedGray",                    
                 "clookGray",                       
                 "concaveTSPRedGray",               
                 "concaveTSPGray",
                 "xTSPRedGray-nearest_insertion",  
                 "xTSPGray-nearest_insertion",      
                 "xTSPRedGray-farthest_insertion", 
                 "xTSPGray-farthest_insertion",  
                 "xTSPRedGray-cheapest_insertion",  
                 "xTSPGray-cheapest_insertion",     
                 "xTSPRedGray-arbitrary_insertion", 
                 "xTSPGray-arbitrary_insertion", 
                 "xTSPRedGray-nn"  ,              
                 "xTSPGray-nn"  ,                   
                 "xTSPRedGray-repetitive_nn",      
                 "xTSPGray-repetitive_nn",
                 "xTSPRedGray-two_opt",             
                 "xTSPGray-two_opt")         



########################################################################################################
#Here list the algos that you  want to see in tables for Tri and Sq grid results
#They must be the same
selectedTriAlgoList <- c(
  "Tri-concaveTSPGray", 
  "Tri-xTSPGray-farthest_insertion",
  "Tri-xTSPGray-nn",
  "Tri-xTSPGray-two_opt"      
)

selectedSqAlgoList <- c(
  "Sq-concaveTSPGray", 
  "Sq-xTSPGray-farthest_insertion",
  "Sq-xTSPGray-nn",
  "Sq-xTSPGray-two_opt"      
)
########################################################################################################



########################################################################################################
# End variables
########################################################################################################


theDF <- resultFile %>% map_df(~read_csv(.))



triDF <- theDF[which(theDF$GridType == "tri"),]
sqDF <- theDF[which(theDF$GridType == "sq"),]


nSim <- length(levels(factor(triDF$BtPosFName)))
cat("Number of sims",nSim,"\n")

nAlgo <- length(levels(factor(triDF$Algo)))
cat("Number of algos",nAlgo/2,"\n")

simlevels <- levels(factor(triDF$BtPosFName))


#Lets find the algonames
nRGAlgos <- length(levels(factor(triDF$Algo)))

theAlgoNames <- theDF$Algo[1:nRGAlgos]

#First the RedGray then the Gray version
theGrayAlgoNames <- theAlgoNames[seq(2,nRGAlgos,by=2)]


theRedGrayAlgoNames <- theAlgoNames[seq(1,nRGAlgos,by=2)]


#These must be the same!!!
nGrayAlgo <- length(theGrayAlgoNames)
nRedGrayAlgo <- nGrayAlgo


nBoatCNT <- theDF$NBoat[1]
#Flag to see if Brute-Force included
bruteforceFlag <- 1
cat("Number of boats",nBoatCNT,"\n")
###############################################################################################
#Triangular grid stats
###############################################################################################

#Get the columns - Triangular grid 
###############################################################################################

cat("\nGenerating tri grid\n")

for (i in (1:nGrayAlgo)) {
  
  #for each col
  #4 of them:
  #cost, AWD,NCharging, Time
  #DEBUG i <- 2
  cat("Generating tri grid table --->",i,"\n")
  
  theRedGrayAlgo <-  theRedGrayAlgoNames[i]
  theGrayAlgo <-  theGrayAlgoNames[i]
  
  
  #Get columns for each algo
  theRedGrayAlgoCostCOL <- triDF[which(triDF$Algo == theRedGrayAlgo),]$Cost
  theRedGrayAlgoAWDCOL <- triDF[which(triDF$Algo == theRedGrayAlgo),]$AWD
  theRedGrayAlgoNChargingCOL <- triDF[which(triDF$Algo == theRedGrayAlgo),]$NCharging
  theRedGrayAlgoTimeCOL <- triDF[which(triDF$Algo == theRedGrayAlgo),]$Time
  
  theGrayAlgoCostCOL <- triDF[which(triDF$Algo == theGrayAlgo),]$Cost
  theGrayAlgoAWDCOL <- triDF[which(triDF$Algo == theGrayAlgo),]$AWD
  theGrayAlgoNChargingCOL <- triDF[which(triDF$Algo == theGrayAlgo),]$NCharging
  theGrayAlgoTimeCOL <- triDF[which(triDF$Algo == theGrayAlgo),]$Time
  
  
  #Now for each algo find savings from RedGray version over Gray only version
  algoCostSavingCOL <- 100 * (theGrayAlgoCostCOL - theRedGrayAlgoCostCOL) / theGrayAlgoCostCOL
  algoAWDSavingCOL <- 100 * (theGrayAlgoAWDCOL - theRedGrayAlgoAWDCOL) / theGrayAlgoAWDCOL
  algoNChargingSavingCOL <- 100 * (theGrayAlgoNChargingCOL - theRedGrayAlgoNChargingCOL) / theGrayAlgoNChargingCOL 
  
  
  AVGalgoCostSaving <- mean(algoCostSavingCOL)
  AVGalgoAWDSaving <- mean(algoAWDSavingCOL)
  AVGalgoNChargingSaving <- mean(algoNChargingSavingCOL)
  
  SDalgoCostSaving <- sd(algoCostSavingCOL)
  SDalgoAWDSaving <- sd(algoAWDSavingCOL)
  SDalgoNChargingSaving <- sd(algoNChargingSavingCOL)
  
  
  if (i==1) {
    
    if (theRedGrayAlgoNames[i]=="permRedGray") {
      #The first row it is the brute force!!!
      
      bruteforceFlag <- 1
      
      #Save the data for speedups and AR
      bruteForceRedGrayCostCOL <- theRedGrayAlgoCostCOL
      bruteForceGrayCostCOL <- theGrayAlgoCostCOL
      
      bruteForceRedGrayTimeCOL <- theRedGrayAlgoTimeCOL
      bruteForceGrayTimeCOL <- theGrayAlgoTimeCOL
      
      
      
      
      #Combine all data and save to a latex table
      nameROW <- c("Algo",
                   "Cost Saving %", 
                   "AWD Saving %", 
                   "Num Chargings Saving %", 
                   "Approx. Ratio RG", 
                   "Speed-up Ratio RG", 
                   "Approx. Ratio G",
                   "Speed-up Ratio G"
      )
      
      
      
      triLatexDF <- as.data.frame(cbind("Tri-BruteForce", 
                                        paste(round( AVGalgoCostSaving,3), "my-pm", round(SDalgoCostSaving,3)),  
                                        paste(round( AVGalgoAWDSaving,3), "my-pm", round(SDalgoAWDSaving,3)),
                                        paste(round( AVGalgoNChargingSaving,3), "my-pm", round(SDalgoNChargingSaving,3)),
                                        NA,
                                        NA,
                                        NA,
                                        NA
      )
      )
      
      colnames(triLatexDF) <- nameROW
      
      
    }else{
      
      #If The first row is not "Brute-force" (permRedGray) then do not calculate ar and speed ups
      bruteforceFlag <- 0
      
      
      #Combine all data and save to a latex table
      nameROW <- c("Algo",
                   "RG Cost",
                   "G Cost",
                   "Cost Saving %",
                   "RG Time",
                   "G Time",
                   "Time Saving %",
                   "RG AWD",
                   "G AWD",
                   "AWD Saving %", 
                   "RG NCharging",
                   "G NCharging",
                   "NCharging Saving %"
                   
      )
      
      
      
      
      
      algoTimeSavingCOL <- 100 * (theGrayAlgoTimeCOL - theRedGrayAlgoTimeCOL) / theGrayAlgoTimeCOL
      AVGalgoTimeSaving <- mean(algoTimeSavingCOL)
      SDalgoTimeSaving <- sd(algoTimeSavingCOL)
      
      triLatexDF <- as.data.frame(cbind(paste0("Tri-",theGrayAlgo),
                                        paste(round(mean(theRedGrayAlgoCostCOL),3), "my-pm", round(sd(theRedGrayAlgoCostCOL),3)), 
                                        paste(round(mean(theGrayAlgoCostCOL),3), "my-pm", round(sd(theGrayAlgoCostCOL),3)), 
                                        paste(round(AVGalgoCostSaving,3), "my-pm", round(SDalgoCostSaving,3)),  
                                        paste(round(mean(theRedGrayAlgoTimeCOL),3), "my-pm", round(sd(theRedGrayAlgoTimeCOL),3)), 
                                        paste(round(mean(theGrayAlgoTimeCOL),3), "my-pm", round(sd(theGrayAlgoTimeCOL),3)),
                                        paste(round(AVGalgoTimeSaving,3), "my-pm", round(SDalgoTimeSaving,3)),
                                        paste(round(mean(theRedGrayAlgoAWDCOL),3), "my-pm", round(sd(theRedGrayAlgoAWDCOL),3)), 
                                        paste(round(mean(theGrayAlgoAWDCOL),3), "my-pm", round(sd(theGrayAlgoAWDCOL),3)), 
                                        paste(round(AVGalgoAWDSaving,3), "my-pm", round(SDalgoAWDSaving,3)),
                                        paste(round(mean(theRedGrayAlgoNChargingCOL),3), "my-pm", round(sd(theRedGrayAlgoNChargingCOL),3)), 
                                        paste(round(mean(theGrayAlgoNChargingCOL),3), "my-pm", round(sd(theGrayAlgoNChargingCOL),3)),
                                        paste(round(AVGalgoNChargingSaving,3), "my-pm", round(SDalgoNChargingSaving,3))
                                        
      )
      )
      
      colnames(triLatexDF) <- nameROW
      
    }
    
  }else{
    
    #The following algorithms
    #Add the following algos by calculating the speedups and Approx ratio
    #Later delete the "Gray" from the name of algo
    
    #Check if there was bruteforce
    if (bruteforceFlag == 1) {
      #We have bruteforce so calculate ar and speedups
      #AR: algoRedGray vs bruteForceRedGray
      theRedGrayAlgoCostAR <- theRedGrayAlgoCostCOL / bruteForceRedGrayCostCOL 
      AVGRedGrayAlgoAR <- mean( theRedGrayAlgoCostAR)
      SDRedGrayAlgoAR <- sd( theRedGrayAlgoCostAR)
      
      
      
      #AR: algoGray vs bruteForceGray
      theGrayAlgoCostAR <- theGrayAlgoCostCOL / bruteForceGrayCostCOL 
      AVGGrayAlgoAR <- mean(theGrayAlgoCostAR)
      SDGrayAlgoAR <- sd(theGrayAlgoCostAR)
      
      
      #Speedup: algoRedGray vs bruteForceRedGray
      theRedGrayAlgoTimeSpeedUp <-  bruteForceRedGrayTimeCOL / theRedGrayAlgoTimeCOL
      AVGalgoRedGraySpeedUp <- mean(theRedGrayAlgoTimeSpeedUp)
      SDalgoRedGraySpeedUp <- sd(theRedGrayAlgoTimeSpeedUp)
      
      
      #Speedup: algoGray vs bruteForceGray
      theGrayAlgoTimeSpeedUp <-  bruteForceGrayTimeCOL / theGrayAlgoTimeCOL
      AVGalgoGraySpeedUp <- mean(theGrayAlgoTimeSpeedUp)
      SDalgoGraySpeedUp <- sd(theGrayAlgoTimeSpeedUp)
      
      
      theNewROW <- as.data.frame(cbind(paste0("Tri-",theGrayAlgo), 
                                       paste(round(AVGalgoCostSaving,3), "my-pm", round(SDalgoCostSaving,3)),  
                                       paste(round(AVGalgoAWDSaving,3), "my-pm", round(SDalgoAWDSaving,3)),
                                       paste(round(AVGalgoNChargingSaving,3), "my-pm", round(SDalgoNChargingSaving,3)),
                                       paste(round(AVGRedGrayAlgoAR,3), "my-pm", round(SDRedGrayAlgoAR,3)),
                                       paste(round(AVGalgoRedGraySpeedUp,3), "my-pm", round(SDalgoRedGraySpeedUp,3)),
                                       paste(round(AVGGrayAlgoAR,3), "my-pm", round(SDGrayAlgoAR,3)),
                                       paste(round(AVGalgoGraySpeedUp,3), "my-pm", round(SDalgoGraySpeedUp,3))
      ))
      
      colnames(theNewROW) <- nameROW
      triLatexDF <- rbind(triLatexDF, theNewROW)
      
    }else{
      
      #We do not have bruteforce so do not calculate ar and speedups
      algoTimeSavingCOL <- 100 * (theGrayAlgoTimeCOL - theRedGrayAlgoTimeCOL) / theGrayAlgoTimeCOL
      AVGalgoTimeSaving <- mean(algoTimeSavingCOL)
      SDalgoTimeSaving <- sd(algoTimeSavingCOL)
      
      theNewROW <- as.data.frame(cbind(paste0("Tri-",theGrayAlgo),
                                       paste(round(mean(theRedGrayAlgoCostCOL),3), "my-pm", round(sd(theRedGrayAlgoCostCOL),3)), 
                                       paste(round(mean(theGrayAlgoCostCOL),3), "my-pm", round(sd(theGrayAlgoCostCOL),3)), 
                                       paste(round(AVGalgoCostSaving,3), "my-pm", round(SDalgoCostSaving,3)),  
                                       paste(round(mean(theRedGrayAlgoTimeCOL),3), "my-pm", round(sd(theRedGrayAlgoTimeCOL),3)), 
                                       paste(round(mean(theGrayAlgoTimeCOL),3), "my-pm", round(sd(theGrayAlgoTimeCOL),3)),
                                       paste(round(AVGalgoTimeSaving,3), "my-pm", round(SDalgoTimeSaving,3)),
                                       paste(round(mean(theRedGrayAlgoAWDCOL),3), "my-pm", round(sd(theRedGrayAlgoAWDCOL),3)), 
                                       paste(round(mean(theGrayAlgoAWDCOL),3), "my-pm", round(sd(theGrayAlgoAWDCOL),3)), 
                                       paste(round(AVGalgoAWDSaving,3), "my-pm", round(SDalgoAWDSaving,3)),
                                       paste(round(mean(theRedGrayAlgoNChargingCOL),3), "my-pm", round(sd(theRedGrayAlgoNChargingCOL),3)), 
                                       paste(round(mean(theGrayAlgoNChargingCOL),3), "my-pm", round(sd(theGrayAlgoNChargingCOL),3)),
                                       paste(round(AVGalgoNChargingSaving,3), "my-pm", round(SDalgoNChargingSaving,3))
                                       
      )
      )
      
      colnames(theNewROW) <- nameROW
      triLatexDF <- rbind(triLatexDF, theNewROW)
    }
  }
}



triMetricPerColumnDF <- triLatexDF



triLatexDFT <- transpose(triLatexDF)
colnames(triLatexDFT) <- triLatexDF$Algo
triLatexDFT <- cbind(colnames(triLatexDF), triLatexDFT)
triAlgoPerColumnDF <- triLatexDFT
colnames(triAlgoPerColumnDF)[1] <- "Algo"
#Filter for the desired algos
triAlgoPerColumnDF <- triAlgoPerColumnDF %>% select("Algo", selectedTriAlgoList)
###############################################################################################

#Print results into latex file 


captionTXT <- paste0("Simulation results for triangular grid.
                          Savings are for Red-Gray heuristics over only Gray edge usage.
                          Approximation ratios and Speed-ups are over Brute-Force method. ",
                     nBoatCNT ," boats randomly generated for ", nSim, " simulations.
                          Results are in the form of mean my-pm standard deviation.")
labelTXT <- paste0("tri-sim",nSim,"bt",nBoatCNT)


# You can choose to print metrics per column table: triMetricPerColumnDF 
results <- xtable::xtable(triAlgoPerColumnDF, 
                          caption = captionTXT,
                          label = labelTXT,
                          NA.string="NA"
)

xtable::print.xtable(results, 
                     type = "latex", 
                     file = paste(myResultDir,paste0(labelTXT,".txt"), sep = "/"), 
                     caption.placement = "top",
                     NA.string="NA",
                     include.rownames=FALSE
)

###############################################################################################







###############################################################################################
#Square grid stats
###############################################################################################

#Get the columns - Square grid 
###############################################################################################
cat("\nGenerating sq grid\n")

for (i in (1:nGrayAlgo)) {
  
  #for each col
  #4 of them:
  #cost, AWD,NCharging, Time
  cat("Generating sq grid table --->",i,"\n")
  
  theRedGrayAlgo <-  theRedGrayAlgoNames[i]
  theGrayAlgo <-  theGrayAlgoNames[i]
  
  
  #Get columns for each algo
  theRedGrayAlgoCostCOL <- sqDF[which(sqDF$Algo == theRedGrayAlgo),]$Cost
  theRedGrayAlgoAWDCOL <- sqDF[which(sqDF$Algo == theRedGrayAlgo),]$AWD
  theRedGrayAlgoNChargingCOL <- sqDF[which(sqDF$Algo == theRedGrayAlgo),]$NCharging
  theRedGrayAlgoTimeCOL <- sqDF[which(sqDF$Algo == theRedGrayAlgo),]$Time
  
  theGrayAlgoCostCOL <- sqDF[which(sqDF$Algo == theGrayAlgo),]$Cost
  theGrayAlgoAWDCOL <- sqDF[which(sqDF$Algo == theGrayAlgo),]$AWD
  theGrayAlgoNChargingCOL <- sqDF[which(sqDF$Algo == theGrayAlgo),]$NCharging
  theGrayAlgoTimeCOL <- sqDF[which(sqDF$Algo == theGrayAlgo),]$Time
  
  
  #Now for each algo find savings from RedGray version over Gray only version
  algoCostSavingCOL <- 100 * (theGrayAlgoCostCOL - theRedGrayAlgoCostCOL) / theGrayAlgoCostCOL
  algoAWDSavingCOL <- 100 * (theGrayAlgoAWDCOL - theRedGrayAlgoAWDCOL) / theGrayAlgoAWDCOL
  algoNChargingSavingCOL <- 100 * (theGrayAlgoNChargingCOL - theRedGrayAlgoNChargingCOL) / theGrayAlgoNChargingCOL 
  
  
  AVGalgoCostSaving <- mean(algoCostSavingCOL)
  AVGalgoAWDSaving <- mean(algoAWDSavingCOL)
  AVGalgoNChargingSaving <- mean(algoNChargingSavingCOL)
  
  SDalgoCostSaving <- sd(algoCostSavingCOL)
  SDalgoAWDSaving <- sd(algoAWDSavingCOL)
  SDalgoNChargingSaving <- sd(algoNChargingSavingCOL)
  
  
  if (i==1) {
    
    if (theRedGrayAlgoNames[i]=="permRedGray") {
      #The first row it is the brute force!!!
      
      bruteforceFlag <- 1
      
      
      #Save the data for speedups and AR
      bruteForceRedGrayCostCOL <- theRedGrayAlgoCostCOL
      bruteForceGrayCostCOL <- theGrayAlgoCostCOL
      
      bruteForceRedGrayTimeCOL <- theRedGrayAlgoTimeCOL
      bruteForceGrayTimeCOL <- theGrayAlgoTimeCOL
      
      
      
      
      #Combine all data and save to a latex table
      nameROW <- c("Algo",
                   "Cost Saving %", 
                   "AWD Saving %", 
                   "Num Chargings Saving %", 
                   "Approx. Ratio RG", 
                   "Speed-up Ratio RG", 
                   "Approx. Ratio G",
                   "Speed-up Ratio G"
      )
      
      
      
      sqLatexDF <- as.data.frame(cbind("Sq-BruteForce", 
                                       paste(round( AVGalgoCostSaving,3), "my-pm", round(SDalgoCostSaving,3)),  
                                       paste(round( AVGalgoAWDSaving,3), "my-pm", round(SDalgoAWDSaving,3)),
                                       paste(round( AVGalgoNChargingSaving,3), "my-pm", round(SDalgoNChargingSaving,3)),
                                       NA,
                                       NA,
                                       NA,
                                       NA
      )
      )
      
      colnames(sqLatexDF) <- nameROW
      
      
    }else{
      #If The first row is not "Brute-force" (permRedGray) then do not calculate ar and speed ups
      bruteforceFlag <- 0
      
      #Combine all data and save to a latex table
      nameROW <- c("Algo",
                   "RG Cost",
                   "G Cost",
                   "Cost Saving %",
                   "RG Time",
                   "G Time",
                   "Time Saving %",
                   "RG AWD",
                   "G AWD",
                   "AWD Saving %", 
                   "RG NCharging",
                   "G NCharging",
                   "NCharging Saving %"
      )
      
      
      
      
      
      algoTimeSavingCOL <- 100 * (theGrayAlgoTimeCOL - theRedGrayAlgoTimeCOL) / theGrayAlgoTimeCOL
      AVGalgoTimeSaving <- mean(algoTimeSavingCOL)
      SDalgoTimeSaving <- sd(algoTimeSavingCOL)
      
      sqLatexDF <- as.data.frame(cbind(paste0("Sq-",theGrayAlgo),
                                       paste(round(mean(theRedGrayAlgoCostCOL),3), "my-pm", round(sd(theRedGrayAlgoCostCOL),3)), 
                                       paste(round(mean(theGrayAlgoCostCOL),3), "my-pm", round(sd(theGrayAlgoCostCOL),3)), 
                                       paste(round(AVGalgoCostSaving,3), "my-pm", round(SDalgoCostSaving,3)),  
                                       paste(round(mean(theRedGrayAlgoTimeCOL),3), "my-pm", round(sd(theRedGrayAlgoTimeCOL),3)), 
                                       paste(round(mean(theGrayAlgoTimeCOL),3), "my-pm", round(sd(theGrayAlgoTimeCOL),3)),
                                       paste(round(AVGalgoTimeSaving,3), "my-pm", round(SDalgoTimeSaving,3)),
                                       paste(round(mean(theRedGrayAlgoAWDCOL),3), "my-pm", round(sd(theRedGrayAlgoAWDCOL),3)), 
                                       paste(round(mean(theGrayAlgoAWDCOL),3), "my-pm", round(sd(theGrayAlgoAWDCOL),3)), 
                                       paste(round(AVGalgoAWDSaving,3), "my-pm", round(SDalgoAWDSaving,3)),
                                       paste(round(mean(theRedGrayAlgoNChargingCOL),3), "my-pm", round(sd(theRedGrayAlgoNChargingCOL),3)), 
                                       paste(round(mean(theGrayAlgoNChargingCOL),3), "my-pm", round(sd(theGrayAlgoNChargingCOL),3)),
                                       paste(round(AVGalgoNChargingSaving,3), "my-pm", round(SDalgoNChargingSaving,3))
                                       
      )
      )
      
      colnames(sqLatexDF) <- nameROW
      
      
      
      
    }
    
  }else{
    #The following algorithms
    #Add the following algos by calculating the speedups and Approx ratio
    #Later delete the "Gray" from the name of algo
    
    
    #Check if there was bruteforce
    if (bruteforceFlag == 1) {
      #We have bruteforce so calculate ar and speedups
      
      #AR: algoRedGray vs bruteForceRedGray
      theRedGrayAlgoCostAR <- theRedGrayAlgoCostCOL / bruteForceRedGrayCostCOL 
      AVGRedGrayAlgoAR <- mean( theRedGrayAlgoCostAR)
      SDRedGrayAlgoAR <- sd( theRedGrayAlgoCostAR)
      
      
      
      #AR: algoGray vs bruteForceGray
      theGrayAlgoCostAR <- theGrayAlgoCostCOL / bruteForceGrayCostCOL 
      AVGGrayAlgoAR <- mean(theGrayAlgoCostAR)
      SDGrayAlgoAR <- sd(theGrayAlgoCostAR)
      
      
      #Speedup: algoRedGray vs bruteForceRedGray
      theRedGrayAlgoTimeSpeedUp <-  bruteForceRedGrayTimeCOL / theRedGrayAlgoTimeCOL
      AVGalgoRedGraySpeedUp <- mean(theRedGrayAlgoTimeSpeedUp)
      SDalgoRedGraySpeedUp <- sd(theRedGrayAlgoTimeSpeedUp)
      
      
      #Speedup: algoGray vs bruteForceGray
      theGrayAlgoTimeSpeedUp <-  bruteForceGrayTimeCOL / theGrayAlgoTimeCOL
      AVGalgoGraySpeedUp <- mean(theGrayAlgoTimeSpeedUp)
      SDalgoGraySpeedUp <- sd(theGrayAlgoTimeSpeedUp)
      
      theNewROW <- as.data.frame(cbind(paste0("Sq-",theGrayAlgo), 
                                       paste(round(AVGalgoCostSaving,3), "my-pm", round(SDalgoCostSaving,3)),  
                                       paste(round(AVGalgoAWDSaving,3), "my-pm", round(SDalgoAWDSaving,3)),
                                       paste(round(AVGalgoNChargingSaving,3), "my-pm", round(SDalgoNChargingSaving,3)),
                                       paste(round(AVGRedGrayAlgoAR,3), "my-pm", round(SDRedGrayAlgoAR,3)),
                                       paste(round(AVGalgoRedGraySpeedUp,3), "my-pm", round(SDalgoRedGraySpeedUp,3)),
                                       paste(round(AVGGrayAlgoAR,3), "my-pm", round(SDGrayAlgoAR,3)),
                                       paste(round(AVGalgoGraySpeedUp,3), "my-pm", round(SDalgoGraySpeedUp,3))
      ))
      
      colnames(theNewROW) <- nameROW
      sqLatexDF <- rbind(sqLatexDF, theNewROW)
      
      
    }else{
      
      
      #We do not have bruteforce so do not calculate ar and speedups
      
      algoTimeSavingCOL <- 100 * (theGrayAlgoTimeCOL - theRedGrayAlgoTimeCOL) / theGrayAlgoTimeCOL
      AVGalgoTimeSaving <- mean(algoTimeSavingCOL)
      SDalgoTimeSaving <- sd(algoTimeSavingCOL)
      
      theNewROW <- as.data.frame(cbind(paste0("Sq-",theGrayAlgo),
                                       paste(round(mean(theRedGrayAlgoCostCOL),3), "my-pm", round(sd(theRedGrayAlgoCostCOL),3)), 
                                       paste(round(mean(theGrayAlgoCostCOL),3), "my-pm", round(sd(theGrayAlgoCostCOL),3)), 
                                       paste(round(AVGalgoCostSaving,3), "my-pm", round(SDalgoCostSaving,3)),  
                                       
                                       paste(round(mean(theRedGrayAlgoTimeCOL),3), "my-pm", round(sd(theRedGrayAlgoTimeCOL),3)), 
                                       paste(round(mean(theGrayAlgoTimeCOL),3), "my-pm", round(sd(theGrayAlgoTimeCOL),3)),
                                       paste(round(AVGalgoTimeSaving,3), "my-pm", round(SDalgoTimeSaving,3)),
                                       
                                       
                                       paste(round(mean(theRedGrayAlgoAWDCOL),3), "my-pm", round(sd(theRedGrayAlgoAWDCOL),3)), 
                                       paste(round(mean(theGrayAlgoAWDCOL),3), "my-pm", round(sd(theGrayAlgoAWDCOL),3)), 
                                       paste(round(AVGalgoAWDSaving,3), "my-pm", round(SDalgoAWDSaving,3)),
                                       
                                       paste(round(mean(theRedGrayAlgoNChargingCOL),3), "my-pm", round(sd(theRedGrayAlgoNChargingCOL),3)), 
                                       paste(round(mean(theGrayAlgoNChargingCOL),3), "my-pm", round(sd(theGrayAlgoNChargingCOL),3)),
                                       paste(round(AVGalgoNChargingSaving,3), "my-pm", round(SDalgoNChargingSaving,3))
                                       
      )
      )
      
      
      colnames(theNewROW) <- nameROW
      sqLatexDF <- rbind(sqLatexDF, theNewROW)
      
    }
  }
  
}




sqMetricPerColumnDF <- sqLatexDF




sqLatexDFT <- transpose(sqLatexDF)
colnames(sqLatexDFT) <- sqLatexDF$Algo
sqLatexDFT <- cbind(colnames(sqLatexDF), sqLatexDFT)
sqAlgoPerColumnDF <- sqLatexDFT
colnames(sqAlgoPerColumnDF)[1] <- "Algo"
#Filter for the desired algos
sqAlgoPerColumnDF <- sqAlgoPerColumnDF %>% select("Algo", selectedSqAlgoList)



###############################################################################################

#Print results into latex file 



captionTXT <- paste0("Simulation results for square grid.
                          Savings are for Red-Gray heuristics over only Gray edge usage.
                          Approximation ratios and Speed-ups are over Brute-Force method. ",
                     nBoatCNT ," boats randomly generated for ", nSim, " simulations.
                          Results are in the form of mean my-pm standard deviation.")
labelTXT <- paste0("sq-sim",nSim,"bt",nBoatCNT)

# You can choose to print metrics per column table: sqMetricPerColumnDF 
results <- xtable::xtable(sqAlgoPerColumnDF , 
                          caption = captionTXT,
                          label = labelTXT,
                          NA.string="NA"
)

xtable::print.xtable(results, 
                     type = "latex", 
                     file = paste(myResultDir,paste0(labelTXT,".txt"), sep = "/"), 
                     caption.placement = "top",
                     NA.string="NA",
                     include.rownames=FALSE
)



###############################################################################################









