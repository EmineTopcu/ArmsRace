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

    for (rng in seq(5, 15, 2))
    {
        InitializeBatDetectRange(rng)
        RunModel("BatRangeDistance", numOfRuns, rng)
        Animate(paste("BatRangeDistance", rng, ".html")) # plot the last one
    }
    write.csv(DF.Stats, "Output/StatsBatRange.csv") #*****************

    for (ang in seq(90, 150, 10))
    {
        InitializeBatDetectAngle(ang)
        RunModel("BatRangeAngle", numOfRuns, ang)
        Animate(paste("BatRangeAngle", ang, ".html")) # plot the last one
    }
    write.csv(DF.Stats, "Output/StatsBatAngle.csv")#*****************
    
    for (rng in seq(50, 100, 10))
    {
        InitializeMothDetectRange(rng)
        RunModel("MothRangeDistance", numOfRuns, rng)
        Animate(paste("MothRangeDistance", rng, ".html")) # plot the last one
    }
    write.csv(DF.Stats, "Output/StatsMothRange.csv")#*****************
    
    for (rng in seq(10, 50, 10))
    {
        InitializeStartleRange(rng)
        RunModel("StartleRange", numOfRuns, rng)
        Animate(paste("StartleRange", rng, ".html")) # plot the last one
    }
    write.csv(DF.Stats, "Output/StatsStartle.csv")#*****************
    
    for (lrn in (1:5))
    {
        InitializeStartleLearning(lrn)
        RunModel("StartleLearning", numOfRuns, lrn)
        Animate(paste("StartleLearning", lrn, ".html")) # plot the last one
    }

    write.csv(DF.Stats, "Output/Stats.csv")
    
    #if (plotResults)
    #{
    #    hist(stats[,'Victim'])
    #    hist(stats[,'Prey'])
    #    hist(stats[,'HuntTime'])
    #}
    #return (stats)
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