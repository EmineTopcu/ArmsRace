## ArmRace.R

library(ggplot2)
library(htmlwidgets)
library(plotly)

source("RangeFunctions.R")
source("MainFunctions.R")
source("Initialize.R")

# Runs the simulation in a specific mode (eg. Bat Distance),
# with the given variable value (eg. 10 m) 
# for numOfRuns of time, 
# and records the outputs to DF.Stats data frame
RunModel <- function (mode, numOfRuns, variable)
{
    for (i in (1:numOfRuns))
    {
        print(paste(mode, variable, i))
        stats <- Simulate()
        DF.Stats <<- rbind(DF.Stats, (as.data.frame(list(Mode = mode, Variable = variable, Seq = i, Victim = stats["Victim"], Prey = stats["Prey"], HuntTime = stats["HuntTime"]))))
    }
}

# The main function that runs the model 
# by adding a new evolutionary step at each scenario
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
    for (rec in (4:1))
    {
        InitializeStartleRecovery(rec)
        RunModel(model, numOfRuns, rec)
        Animate(paste(model, rec, ".html")) # plot the last one
    }

    model <- "StartleLearning"
    for (lrn in (5:2))
    {
        InitializeStartleLearning(lrn)
        RunModel(model, numOfRuns, lrn)
        Animate(paste(model, lrn, ".html")) # plot the last one
    }
    write.csv(DF.Stats, "Output/Stats.csv")
}

# Run the simulation once with default parameters
NormalRun <- function()
{
    set.seed(0)
    InitializeParameters()
    
    DF.Stats <<-  data.frame(matrix(ncol = 6, nrow = 0))
    colnames(DF.Stats) <- c('Mode', 'Variable', 'Seq', 'Victim', 'Prey', 'HuntTime')
    
    RunModel("NormalRun", 1, 0)
    Animate("NormalRun.html") # plot the last one
}

# Run sensitivity analysis by keeping everything default
# and removing one evolutionary event
SensitivityAnalysis <- function()
{
    numOfRuns <- 30
    
    DF.Stats <<-  data.frame(matrix(ncol = 6, nrow = 0))
    colnames(DF.Stats) <<- c('Mode', 'Variable', 'Seq', 'Victim', 'Prey', 'HuntTime')

    set.seed(0)
    InitializeParameters()
    Param.TimeRange <<- 300
    RunModel("Normal Hunt", numOfRuns, 0)

    set.seed(0)
    InitializeParameters()
    Param.TimeRange <<- 300
    Param.BatRangeDist <<- 0
    RunModel("No Bat Detection", numOfRuns, 0)

    set.seed(0)
    InitializeParameters()
    Param.TimeRange <<- 300
    Param.MothRange <<- 0
    RunModel("No Moth Detection", numOfRuns, 0)
    
    set.seed(0)
    InitializeParameters()
    Param.TimeRange <<- 300
    Param.StartleRange <<- 0
    RunModel("No Startle", numOfRuns, 0)
    
    set.seed(0)
    InitializeParameters()
    Param.TimeRange <<- 300
    Param.LearnTime <<- 999
    RunModel("No Learning", numOfRuns, 999)
    
    set.seed(0)
    InitializeParameters()
    Param.TimeRange <<- 300
    Param.DangerZone <<- 0.1
    RunModel("Min DangerZone", numOfRuns, 0)
    
    write.csv(DF.Stats, "Output/SensitivityStats.csv")
    
}



#Main() # uncomment to run the model

#NormalRun() # uncomment to run single run with default values

#SensitivityAnalysis() # uncomment to do sensitivity analysis
