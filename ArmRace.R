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
        print(paste(mode, variable, i))
        stats <- Simulate()
        DF.Stats <<- rbind(DF.Stats, (as.data.frame(list(Mode = mode, Variable = variable, Seq = i, Victim = stats["Victim"], Prey = stats["Prey"], HuntTime = stats["HuntTime"]))))
    }
}

Main <- function()
{
    set.seed(0)
    numOfRuns <- 100
    InitializeParameters()
    
    DF.Stats <<-  data.frame(matrix(ncol = 6, nrow = 0))
    colnames(DF.Stats) <<- c('Mode', 'Variable', 'Seq', 'Victim', 'Prey', 'HuntTime')

    InitializeRandomHunt()
    RunModel("RandomHunt", numOfRuns, 0)
    Animate("RandomHunt.html") # plot the last one
    
    model <- "BatRangeDistance"
    for (rng in seq(5, 20, 5))
    {
        InitializeBatDetectRange(rng)
        RunModel(model, numOfRuns, rng)
        Animate(paste(model, rng, ".html")) # plot the last one
    }

    model <- "BatRangeAngle"
    for (ang in seq(90, 150, 20))
    {
        InitializeBatDetectAngle(ang)
        RunModel(model, numOfRuns, ang)
        Animate(paste(model, ang, ".html")) # plot the last one
    }

    model <- "MothRangeDistance"
    for (rng in seq(60, 120, 20))
    {
        InitializeMothDetectRange(rng)
        RunModel(model, numOfRuns, rng)
        Animate(paste(model, rng, ".html")) # plot the last one
    }
    
    model <- "StartleRange"
    for (rng in seq(10, 40, 10))
    {
        InitializeStartleRange(rng)
        RunModel(model, numOfRuns, rng)
        Animate(paste(model, rng, ".html")) # plot the last one
    }

    model <- "StartleRecovery"
    for (rec in seq(1, 4, 1))
    {
        InitializeStartleRecovery(rec)
        RunModel(model, numOfRuns, rec)
        Animate(paste(model, rec, ".html")) # plot the last one
    }

    model <- "StartleLearning"
    for (lrn in (2:5))
    {
        InitializeStartleLearning(lrn)
        RunModel(model, numOfRuns, lrn)
        Animate(paste(model, lrn, ".html")) # plot the last one
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