

BatCatchesMoth <- function(batseq, mothseq, t)
{
    DF.LunchTime <<-  rbind(DF.LunchTime, (as.data.frame(list(MothID=mothseq, BatID=batseq, EatenAt = t))))
    
    DF.Animals[DF.Animals$Animal == 'Moth' & DF.Animals$ID == mothseq, "X"] <<- - Param.Width
    DF.Animals[DF.Animals$Animal == 'Moth' & DF.Animals$ID == mothseq, "Y"] <<- - Param.Height
    DF.Animals[DF.Animals$Animal == 'Moth' & DF.Animals$ID == mothseq, "Velocity"] <<- 0
}

MothStartlesBat <- function(batseq, t)
{
    DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "Velocity"] <<- 0
    DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "LastStartled"] <<- t
    numstartled <- DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "NumStartled"]
    DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "NumStartled"] <<- numstartled + 1
}

BatRecoversFromStartle <- function(batseq, t)
{
    DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "Velocity"] <<- rnorm(1, Param.VelBat, Param.VelBatSD)
    DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "LastStartled"] <<- 0
}

TimeIncr <- function(t)
{
    for (i in (1:nrow(DF.Animals)))
    {
        if (DF.Animals$Velocity[i] > 0)
        {
            ns <- NextStep(DF.Animals$X[i], DF.Animals$Y[i], DF.Animals$Angle[i], DF.Animals$Velocity[i], Param.dt)
            DF.Animals$X[i] <<- ns[1]
            DF.Animals$Y[i] <<- ns[2]
            DF.Animals$Angle[i] <<- ns[3]
            DF.Trace <<- rbind(DF.Trace, as.data.frame(list(Animal = DF.Animals$Animal[i], ID=DF.Animals$ID[i], Time = t, X = ns[1], Y = ns[2])))
        }
        else if (DF.Animals$Animal[i] == 'Moth') # Valeocity of eaten moths are set to 0
        {
            ID <- DF.Animals$ID[i]
            masterseq <- DF.LunchTime[DF.LunchTime$MothID == ID, 'BatID']
            masterpos <- DF.Animals[DF.Animals$Animal == 'Bat' & DF.Animals$ID == masterseq,]
            DF.Trace <<- rbind(DF.Trace, as.data.frame(list(Animal = DF.Animals$Animal[i], ID = DF.Animals$ID[i], Time = t, X = masterpos$X, Y = masterpos$Y))) 
        }
        else # Bat with velocity = 0 means it is startled
        {
            DF.Animals$X[i] <<- rnorm(1, DF.Animals$X[i], 0.1)
            DF.Animals$Y[i] <<- rnorm(1, DF.Animals$Y[i], 0.1)
            DF.Animals$Angle[i] <<- rnorm(1, DF.Animals$Angle[i], 1)
            DF.Trace <<- rbind(DF.Trace, as.data.frame(list(Animal = DF.Animals$Animal[i], ID=DF.Animals$ID[i], Time = t, X = DF.Animals$X[i], Y = DF.Animals$Y[i])))
        }
    }
    moth_df <- DF.Animals[DF.Animals$Animal == 'Moth', ]
    bat_df <- DF.Animals[DF.Animals$Animal == 'Bat',]
    for (i in (1:nrow(moth_df)))
    {
        mothx <-  moth_df$X[i]
        mothy <-  moth_df$Y[i]
        mothseq <- moth_df$ID[i]
        for (j in (1:nrow(bat_df)))
        {
            batx <-  bat_df$X[j]
            baty <-  bat_df$Y[j]
            batseq <- bat_df$ID[j]
            batangle <- bat_df$Angle[j]
            batlaststartled <- bat_df$LastStartled[j]
            batnumstartled <- bat_df$NumStartled[j]
            
            dist <- CalcDist(mothx, mothy, batx, baty)
            moth_detected <- WithinRange(batx, baty, mothx, mothy, Param.BatRangeDist, batangle, Param.BatRangeAngle)
            if (batlaststartled > 0)
            { 
                if ((t - batlaststartled) >= Param.RecoveryTime)
                {
                    BatRecoversFromStartle(batseq, t)
                    batstartled <- 0
                }
            }
            else if (dist < Param.StartleRange & batnumstartled <= Param.LearnTime)
            {
                batlaststartled <- t
                MothStartlesBat(batseq, t)
            }
            if (batlaststartled == 0 & moth_detected) # bat detects moths, turns towards
            {
                alpha <- CalcAngle(batx, baty, mothx, mothy)
                DF.Animals[DF.Animals$Animal =='Bat' & DF.Animals$ID == batseq, "Angle"] <<- alpha
            }
            
            if (dist <= Param.DangerZone) # moth gets eaten
            {
                BatCatchesMoth (bat_df$ID[j], mothseq, t) 
            }
            else if (dist < Param.MothRange) # moth detects bat, turns back
            {
                alpha <- CalcAngle(batx, baty, mothx, mothy)
                DF.Animals[DF.Animals$Animal =='Moth' & DF.Animals$ID == mothseq, "Angle"] <<- alpha
            }
        }  
    }
}

Simulate <- function()
{
    InitializeDataFrames()
    InitializeAnimals(Param.NumOfBats, Param.VelBat, Param.VelBatSD, 'Bat')
    InitializeAnimals(Param.NumOfMoths, Param.VelMoth, Param.VelMothSD, 'Moth')
    
    timerange <- (Param.Height**2 + Param.Width**2)^0.5 / Param.VelMoth * 2
    stats <- c('Victim' = 0, 'Prey' = 0, 'HuntTime' = 0)
    for (i in seq(1, timerange, Param.dt))
    {
        TimeIncr(i)
    }
    stats["Victim"] <- nrow(DF.LunchTime) / Param.NumOfMoths
    stats["Prey"] <- nrow(DF.LunchTime) / Param.NumOfBats
    stats["HuntTime"] <- mean(DF.LunchTime$EatenAt) / timerange
    return (stats)
}


Animate <- function(fileName = NULL)
{
    fileName <- gsub(" ", "", paste('Output/',fileName))
    p <- ggplot(DF.Trace, aes(X, Y, frame = Time)) +
        xlim(0, Param.Width) + ylim(0, Param.Height) + theme_bw() +
        geom_point(aes(size = Animal, shape = Animal, color = Animal, frame = Time, ids = paste(Animal, ID))) +
        scale_size_manual(values=c(5, 2)) +
        scale_shape_manual(values=c(11, 5)) +
        scale_color_manual(values=c("black", "navajowhite4"))
    
    pp <- ggplotly(p) %>% animation_opts(frame = 250, easing = "linear", redraw = FALSE)
    pp
    if (! is.null(fileName))
        saveWidget(pp, file = fileName, selfcontained = TRUE)
}