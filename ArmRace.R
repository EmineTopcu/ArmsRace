library(ggplot2)
library(htmlwidgets)
library(plotly)

source("RangeFunctions.R")
source("MainFunctions.R")
source("Initialize.R")


RunModel <- function (mode, numOfRuns, variable)
{
    for (i in (1:numOfRuns))
    {
        print(paste(mode, i))
        stats <- Simulate()
        DF.Stats <<- rbind(DF.Stats, (as.data.frame(list(Mode = mode, Variable = variable, Seq = i, Victim = stats["Victim"], Prey = stats["Prey"], HuntTime = stats["HuntTime"]))))
    }
}

Main <- function()
{
    numOfRuns <- 100
    InitializeParameters()
    
    DF.Stats <<-  data.frame(matrix(ncol = 6, nrow = 0))
    colnames(DF.Stats) <<- c('Mode', 'Variable', 'Seq', 'Victim', 'Prey', 'HuntTime')
    
    InitializeRandomHunt()
    RunModel("RandomHunt", numOfRuns, 0)
    Animate("RandomHunt.html") # plot the last one
    write.csv(DF.Stats, "Output/StatsRandom.csv") #*****************
    
    for (rng in seq(5, 20, 5))
    {
        model <- paste("BatRangeDistance", rng)
        InitializeBatDetectRange(rng)
        RunModel(model, numOfRuns, rng)
        Animate(paste(model, ".html")) # plot the last one
    }
    write.csv(DF.Stats, "Output/StatsBatRange.csv") #*****************

    for (ang in seq(90, 150, 20))
    {
        model <- paste("BatRangeAngle", ang)
        InitializeBatDetectAngle(ang)
        RunModel(model, numOfRuns, ang)
        Animate(paste(model, ".html")) # plot the last one
    }
    write.csv(DF.Stats, "Output/StatsBatAngle.csv")#*****************

    for (rng in seq(60, 120, 20))
    {
        model <- paste("MothRangeDistance", rng)
        InitializeMothDetectRange(rng)
        RunModel(model, numOfRuns, rng)
        Animate(paste(model, ".html")) # plot the last one
    }
    write.csv(DF.Stats, "Output/StatsMothRange.csv")#*****************

    for (rng in seq(10, 40, 10)) 
    {
        model <- paste("StartleRange", rng)
        InitializeStartleRange(rng)
        RunModel(model, numOfRuns, rng)
        Animate(paste(model, ".html")) # plot the last one
    }
    write.csv(DF.Stats, "Output/StatsStartle.csv")#*****************

    for (lrn in (2:5))
    {
        model <- paste("StartleLearning", lrn)
        InitializeStartleLearning(lrn)
        RunModel(model, numOfRuns, lrn)
        Animate(paste(model, ".html")) # plot the last one
    }

    write.csv(DF.Stats, "Output/Stats.csv")
}

NormalRun <- function()
{
    InitializeParameters()
    
    DF.Stats <<-  data.frame(matrix(ncol = 6, nrow = 0))
    colnames(DF.Stats) <- c('Mode', 'Variable', 'Seq', 'Victim', 'Prey', 'HuntTime')
    
    RunModel("NormalRun", 1, 0)
    Animate("NormalRun.html") # plot the last one
    
}
Main()

#NormalRun()