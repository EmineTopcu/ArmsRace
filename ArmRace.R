library(ggplot2)
library(htmlwidgets)
library(plotly)

source("RangeFunctions.R")
source("MainFunctions.R")



InitializeRandomHunt <- function()
{
    Param.MothRange <<- 0 # moth cannot detect bats
    Param.BatRangeDist <<- 0 # bat cannot detect moths
    Param.StartleRange <<- 0
    Param.LearnTime <<- 999
}

InitializeBatDetectRange <- function(rng)
{
    Param.BatRangeDist <<- rng
    Param.BatRangeAngle <<- 90
    Param.MothRange <<- 100
    Param.StartleRange <<- 0
    Param.LearnTime <<- 999
}

InitializeBatDetectAngle <- function(rng)
{
    Param.BatRangeDist <<- 15
    Param.BatRangeAngle <<- rng
    Param.MothRange <<- 0 # moth cannot detect bats
    Param.StartleRange <<- 0
    Param.LearnTime <<- 999
}

InitializeMothDetectRange <- function(rng)
{
    Param.BatRangeDist <<- 15
    Param.BatRangeAngle <<- 150
    Param.MothRange <<- rng
    Param.StartleRange <<- 0
    Param.LearnTime <<- 999
}

InitializeStartleRange <- function(rng)
{
    Param.BatRangeDist <<- 15
    Param.BatRangeAngle <<- 150
    Param.MothRange <<- 100
    Param.StartleRange <<- rng
    Param.LearnTime <<- 999
}

InitializeStartleLearning <- function(lrn)
{
    Param.BatRangeDist <<- 15
    Param.BatRangeAngle <<- 150
    Param.MothRange <<- 100
    Param.StartleRange <<- 50
    Param.LearnTime <<- lrn
}


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
    numOfRuns <- 2
    InitializeParameters()
    
    DF.Stats <<-  data.frame(matrix(ncol = 6, nrow = 0))
    colnames(DF.Stats) <<- c('Mode', 'Variable', 'Seq', 'Victim', 'Prey', 'HuntTime')
    
    InitializeRandomHunt()
    RunModel("RandomHunt", numOfRuns, 0)
    Animate("RandomHunt.html") # plot the last one

    for (rng in (5:6))
    {
        InitializeBatDetectRange(rng)
        RunModel("BatRangeDistance", numOfRuns, rng)
        Animate(paste("BatRangeDistance", rng, ".html")) # plot the last one
    }

    for (ang in seq(90, 100, 10))
    {
        InitializeBatDetectAngle(ang)
        RunModel("BatRangeAngle", numOfRuns, ang)
        Animate(paste("BatRangeAngle", ang, ".html")) # plot the last one
    }
    
    for (rng in seq(90, 100, 10))
    {
        InitializeMothDetectRange(rng)
        RunModel("MothRangeDistance", numOfRuns, rng)
        Animate(paste("MothRangeDistance", rng, ".html")) # plot the last one
    }
    
    for (rng in seq(10, 50, 10))
    {
        InitializeStartleRange(rng)
        RunModel("StartleRange", numOfRuns, rng)
        Animate(paste("StartleRange", rng, ".html")) # plot the last one
    }
    
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

Main()

