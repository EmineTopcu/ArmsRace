
InitializeParameters <- function()
{
    Param.Height <<- 300
    Param.Width <<- 300
    Param.dt <<- 0.1
    
    Param.NumOfBats <<- 3
    Param.VelBat <<- 10 
    Param.VelBatSD <<- 2
    Param.NumOfMoths <<- 10
    Param.VelMoth <<- 5
    Param.VelMothSD <<- 1
    
    Param.DangerZone <<- 1
    
    Param.BatRangeDist <<- 15
    Param.BatRangeAngle <<- 150
    Param.MothRange <<- 100 # the range that a moth detects the presence of the moth
    Param.StartleRange <<- 20 # the range where the bat hears moth cry and startles
    Param.RecoveryTime <<-  # how many seconds the bat is startled
    Param.LearnTime <<- 3 # how many times it takes for the bat to learn to ignore the moth cry and not startle
}

InitializeDataFrames <- function()
{
    DF.Animals <<- data.frame(matrix(ncol = 8, nrow = 0))
    colnames(DF.Animals) <<- c('Animal', 'ID', 'X', 'Y', 'Angle', 'Velocity', 'NumStartled', 'LastStartled')
    
    DF.LunchTime <<- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(DF.LunchTime) <<- c('MothID', 'BatID', 'EatenAt')
    
    DF.Trace <<- data.frame(matrix(ncol = 5, nrow = 0))
    colnames(DF.Trace) <<- c('Animal', 'ID', 'Time', 'X', 'Y')
}

InitializeAnimals <- function(num, meanVel, sdVel, bm)
{
    y <- runif(num, 1, Param.Height)
    x <- runif(num, 1, Param.Width)
    angle <- runif(num,0, 360)
    vel <- rnorm(num, meanVel, sdVel)
    DF.Animals <<- rbind(DF.Animals, (as.data.frame(list(Animal=rep(bm, num), ID=(1:num), X = x, Y = y, Angle = angle, Velocity = vel, NumStartled = 0, LastStartled = 0))))
}


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
