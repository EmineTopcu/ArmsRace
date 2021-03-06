## MainFunctions.R
## The main functions of the simulation

# When bat catches moth:
# the predation information is logged in DF.Predation dataframe
# the velocity and the position of moth is cleared
BatCatchesMoth <- function(batseq, mothseq, t)
{
    DF.Predation <<-  rbind(DF.Predation, (as.data.frame(list(MothID=mothseq, BatID=batseq, EatenAt = t))))
    
    DF.Animals[DF.Animals$Animal == 'Moth' & DF.Animals$ID == mothseq, "X"] <<- - Param.Width
    DF.Animals[DF.Animals$Animal == 'Moth' & DF.Animals$ID == mothseq, "Y"] <<- - Param.Height
    DF.Animals[DF.Animals$Animal == 'Moth' & DF.Animals$ID == mothseq, "Velocity"] <<- 0
}

# When moth startles bat:
# the starling information is logged to be used by recovery and learning behaviors
# the velocity of the bat is zeroed out
MothStartlesBat <- function(mothseq, batseq, t)
{
    DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "Velocity"] <<- 0
    DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "LastStartled"] <<- t
    DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "LastStartledBy"] <<- mothseq
    numstartled <- DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "NumStartled"]
    DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "NumStartled"] <<- numstartled + 1
}

# When bat recovers from startling:
# the bat continue to fly at its original direction
BatRecoversFromStartle <- function(batseq, t)
{
    DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "Velocity"] <<- rnorm(1, Param.VelBat, Param.VelBatSD)
    DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "LastStartled"] <<- 0
}

# checks whether a moth is already hunted by a bat
# to prevent multiple bats hunting the same moth
MasterOfMoth <- function(mothseq)
{
    masterseq <- DF.Predation[DF.Predation$MothID == mothseq, 'BatID']
    return (masterseq[1])
}

# the main function simulating each time increment
# the position, velocity, and fate of each animal is calculated based on 
# its previous location, its proximity to the borders of the area
# and its proximity to a predator or a prey
TimeIncr <- function(t)
{
    for (i in (1:nrow(DF.Animals)))
    {
        if (DF.Animals$Velocity[i] > 0.01)
        {
            isMoth <- DF.Animals$Animal[i] == 'Moth'
            ns <- NextStep(DF.Animals$X[i], DF.Animals$Y[i], DF.Animals$Angle[i], DF.Animals$Velocity[i], Param.dt)
            DF.Animals$X[i] <<- ns[1]
            DF.Animals$Y[i] <<- ns[2]
            DF.Animals$Angle[i] <<- ns[3]
            DF.Trace <<- rbind(DF.Trace, as.data.frame(list(Animal = DF.Animals$Animal[i], ID=DF.Animals$ID[i], Time = t, X = ns[1], Y = ns[2])))
        }
        else if (DF.Animals$Animal[i] == 'Moth') # Velocity of eaten moths are set to 0
        {
            ID <- DF.Animals$ID[i]
            masterseq <- DF.Predation[DF.Predation$MothID == ID, 'BatID']
            masterpos <- DF.Animals[DF.Animals$Animal == 'Bat' & DF.Animals$ID == masterseq,]
            DF.Trace <<- rbind(DF.Trace, as.data.frame(list(Animal = DF.Animals$Animal[i], ID = DF.Animals$ID[i], Time = t, X = masterpos$X, Y = masterpos$Y))) 
        }
        else # Bat with velocity = 0 means it is startled
        {
            x <- rnorm(1, DF.Animals$X[i], 0.1)
            if (x < 0) x <- 0.1
            else if (x > Param.Width) x <- Param.Width - 0.1
            y <- rnorm(1, DF.Animals$Y[i], 0.1)
            if (y < 0) y <- 0.1
            else if (y > Param.Height) y <- Param.Height - 0.1
            DF.Animals$X[i] <<- x
            DF.Animals$Y[i] <<- y
            DF.Animals$Angle[i] <<- rnorm(1, DF.Animals$Angle[i], 1)
            DF.Trace <<- rbind(DF.Trace, as.data.frame(list(Animal = DF.Animals$Animal[i], ID=DF.Animals$ID[i], Time = t, X = DF.Animals$X[i], Y = DF.Animals$Y[i])))
        }
    }
    moth_df <- DF.Animals[DF.Animals$Animal == 'Moth', ]
    bat_df <- DF.Animals[DF.Animals$Animal == 'Bat',]
    # bats are iterated in random to create stochasticity 
    # to prevent bias to certain bats if multiple bats detect a moth
    for (i in sample(1:Param.NumOfMoths, Param.NumOfMoths, replace = FALSE)) 
    {
        mothx <-  moth_df$X[i]
        mothy <-  moth_df$Y[i]
        mothseq <- moth_df$ID[i]
        for (j in sample(1:Param.NumOfBats, Param.NumOfBats, replace = FALSE))
        {
            batx <-  bat_df$X[j]
            baty <-  bat_df$Y[j]
            batseq <- bat_df$ID[j]
            batangle <- bat_df$Angle[j]
            batlaststartled <- bat_df$LastStartled[j]
            batlaststartledby <- bat_df$LastStartledBy[j]
            batnumstartled <- bat_df$NumStartled[j]
            dist <- CalcDist(mothx, mothy, batx, baty)
            moth_detected <- WithinRange(batx, baty, mothx, mothy, Param.BatRangeDist, batangle, Param.BatRangeAngle)
            if (batlaststartledby == mothseq & batlaststartled > 0)
            { 
                if ((t - batlaststartled) >= Param.RecoveryTime)
                {
                    BatRecoversFromStartle(batseq, t)
                    batstartled <- 0
                }
            }
            # if bat is within the moth's startle range, 
            # and has not reached the learning number of startles,
            # and was not startled by the same moth before (to prevent multiple startles for the same hunt)
            else if (dist < Param.StartleRange & batnumstartled <= Param.LearnTime & batlaststartledby != mothseq)
            {
                batlaststartled <- t
                MothStartlesBat(mothseq, batseq, t)
            }
            #clear laststartledby info so that bat can be startled if met by the same moth again
            else if (batlaststartledby == mothseq & dist > Param.StartleRange)
            {
                batlaststartledby <- 0
                DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "LastStartledBy"] <<- 0
            }
            
            if (batlaststartled == 0 & moth_detected) # bat detects moths, turns towards
            {
                alpha <- CalcAngle(batx, baty, mothx, mothy, randomize = TRUE)
                DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "Angle"] <<- alpha
            }
            
            if (dist <= Param.DangerZone) # moth gets eaten
            {
                if (is.na(MasterOfMoth(mothseq))) # to prevent to be eaten by two bats
                    BatCatchesMoth (bat_df$ID[j], mothseq, t) 
            }
            else if (dist < Param.MothRange) # moth detects bat, turns back
            {
                alpha <- CalcAngle(batx, baty, mothx, mothy, randomize = TRUE)
                DF.Animals[DF.Animals$Animal =='Moth' & DF.Animals$ID == mothseq, "Angle"] <<- alpha
            }
        }  
    }
}

# Run a simulation using the global modelling parameters
# by discrete increments of time
Simulate <- function()
{
    InitializeDataFrames()
    InitializeAnimals(Param.NumOfBats, Param.VelBat, Param.VelBatSD, 'Bat', 0, Param.Width / 4)
    InitializeAnimals(Param.NumOfMoths, Param.VelMoth, Param.VelMothSD, 'Moth', Param.Width / 4, Param.Width)
    
    stats <- c('Victim' = 0, 'Prey' = 0, 'HuntTime' = 0)
    for (i in seq(1, Param.TimeRange, Param.dt))
    {
        TimeIncr(i)
    }
    stats["Victim"] <- nrow(DF.Predation) / Param.NumOfMoths
    stats["Prey"] <- nrow(DF.Predation) / Param.NumOfBats
    stats["HuntTime"] <- mean(DF.Predation$EatenAt) 
    return (stats)
}

# Create the animation file using the plotly package
Animate <- function(fileName = NULL)
{
    fileName <- gsub(" ", "", paste('Output/',fileName))
    p <- ggplot(DF.Trace, aes(X, Y, frame = Time)) +
        xlim(0, Param.Width) + ylim(0, Param.Height) + theme_bw() +
        geom_point(aes(size = Animal, shape = Animal, color = Animal, frame = Time, ids = paste(Animal, ID))) +
        scale_size_manual(values=c(5, 2)) +
        scale_shape_manual(values=c(11, 5)) +
        scale_color_manual(values=c("black", "navajowhite4"))
    
    anim <- ggplotly(p) %>% animation_opts(frame = 50, easing = "linear", redraw = FALSE)
    if (! is.null(fileName)) # save the animation as an html file
        saveWidget(anim, file = fileName, selfcontained = TRUE)
}