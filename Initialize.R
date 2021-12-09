## Initialize.R
## This file conatins the initialization of the global modelling parameters,
## data frames, 
## initial bat and moth placements and velocities for each scenario 

# Initialize global modelling parameters
InitializeParameters <- function()
{
    Param.Height <<- 300 # the width of the simulation area in meters
    Param.Width <<- 300 # the height of the simulation area in meters
    Param.dt <<- 0.1 # the unit of time in seconds
    
    Param.NumOfBats <<- 3 # the number of bats simulated
    Param.VelBat <<- 10 # the average bat velocity in m/s
    Param.VelBatSD <<- 2 # the standard deviation bat velocity
    Param.NumOfMoths <<- 10 # the number of moths simulated
    Param.VelMoth <<- 1 # the average moth velocity in m/s
    Param.VelMothSD <<- 0.1 # the standard deviation bat velocity
    
    Param.DangerZone <<- 1 # the distance where the moth is eaten by the bat
    
    Param.BatRangeDist <<- 15 # the range the bat can detect a moth in meters
    Param.BatRangeAngle <<- 120 # the angle the bat can detect a moth
    Param.MothRange <<- 100 # the range that a moth detects a bat
    Param.StartleRange <<- 20 # the range bat hears moth cry and startles
    Param.RecoveryTime <<- 3 # how many seconds the bat is startled for
    Param.LearnTime <<- 1 # # of startling times required to learn
    
    # The run time is the time for a moth to cross the area diagonally twice
    Param.TimeRange <<- (Param.Height**2 + Param.Width**2)^0.5 / Param.VelMoth * 2
}

# Initialize the data frames that keeps information of the current simulation run
InitializeDataFrames <- function()
{
    # the data frame that keeps the current information of each animal
    DF.Animals <<- data.frame(matrix(ncol = 9, nrow = 0))
    colnames(DF.Animals) <<- c('Animal', 'ID', 'X', 'Y', 'Angle', 'Velocity', 'NumStartled', 'LastStartled', 'LastStartledBy')
    
    # the data frame that keeps track of all predation events
    DF.Predation <<- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(DF.Predation) <<- c('MothID', 'BatID', 'EatenAt')
    
    # the data frame that keeps tha trail of each animal
    # throughout the current simulation run
    DF.Trace <<- data.frame(matrix(ncol = 5, nrow = 0))
    colnames(DF.Trace) <<- c('Animal', 'ID', 'Time', 'X', 'Y')
}

# Generate animals and set their initial conditions
InitializeAnimals <- function(num, meanVel, sdVel, bm, minX, maxX)
{
    y <- runif(num, 1, Param.Height)
    x <- runif(num, minX, maxX)
    angle <- runif(num,0, 360)
    vel <- rnorm(num, meanVel, sdVel)
    DF.Animals <<- rbind(DF.Animals, (as.data.frame(list(Animal=rep(bm, num), ID = (1:num), X = x, Y = y, Angle = angle, Velocity = vel, NumStartled = 0, LastStartled = 0, LastStartledBy = 0))))
}

# the below functions are called from each scenario 
# to override the modelling parameters specific to that scenario
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
    Param.BatRangeAngle <<- 120
    Param.MothRange <<- 0 # moth cannot detect bats
    Param.StartleRange <<- 0 # no moth cries to startle bats
    Param.LearnTime <<- 999
}

InitializeBatDetectAngle <- function(rng)
{
    Param.BatRangeDist <<- 15
    Param.BatRangeAngle <<- rng
    Param.MothRange <<- 0 # moth cannot detect bats
    Param.StartleRange <<- 0 # no moth cries to startle bats
    Param.LearnTime <<- 999
}

InitializeMothDetectRange <- function(rng)
{
    Param.BatRangeDist <<- 15
    Param.BatRangeAngle <<- 120
    Param.MothRange <<- rng
    Param.StartleRange <<- 0 # no moth cries to startle bats
    Param.LearnTime <<- 999 
}

InitializeStartleRange <- function(rng)
{
    Param.BatRangeDist <<- 15
    Param.BatRangeAngle <<- 120
    Param.MothRange <<- 100
    Param.StartleRange <<- rng
    Param.LearnTime <<- 999 # bat continues to startle with each moth cry - no learning
}

InitializeStartleRecovery <- function(rec)
{
    Param.BatRangeDist <<- 15
    Param.BatRangeAngle <<- 120
    Param.MothRange <<- 100
    Param.StartleRange <<- 50
    Param.StartleRecovery <<- rec
    Param.LearnTime <<- 3
}

InitializeStartleLearning <- function(lrn)
{
    Param.BatRangeDist <<- 15
    Param.BatRangeAngle <<- 120
    Param.MothRange <<- 100
    Param.StartleRange <<- 50
    Param.LearnTime <<- lrn
}
