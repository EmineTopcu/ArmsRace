
InitializeParameters <- function()
{
    Param.Height <<- 150
    Param.Width <<- 150
    Param.dt <<- 0.1

    Param.DangerZone <<- 1
    Param.BatRangeDist <<- 15
    Param.BatRangeAngle <<- 150
    Param.MothRange <<- 100
    
    Param.NumOfBats <<- 3
    Param.VelBat <<- 10 
    Param.VelBatSD <<- 2
    Param.NumOfMoths <<- 10
    Param.VelMoth <<- 5
    Param.VelMothSD <<- 1
}

InitializeAnimals <- function(num, meanVel, sdVel, bm)
{
    y <- runif(num, 1, Param.Height)
    x <- runif(num, 1, Param.Width)
    angle <- runif(num,0, 360)
    vel <- rnorm(num, meanVel, sdVel)
    df_animals <<- rbind(df_animals, (as.data.frame(list(bm=rep(bm, num), seq=(1:num), x = x, y = y, angle = angle, vel = vel))))
}


BatCatchesMoth <- function(batseq, mothseq, t)
{
    df_lunchtime <<-  rbind(df_lunchtime, (as.data.frame(list(moth_seq=mothseq, bat_seq=batseq, eaten_at = t))))
    
    df_animals[df_animals$bm=='Moth' & df_animals$seq == mothseq, "x"] <<- - Param.Width
    df_animals[df_animals$bm=='Moth' & df_animals$seq == mothseq, "y"] <<- - Param.Height
    df_animals[df_animals$bm=='Moth' & df_animals$seq == mothseq, "vel"] <<- 0
}


TimeIncr <- function(t)
{
    for (i in (1:nrow(df_animals)))
    {
        if (df_animals$vel[i] > 0)
        {
            ns <- NextStep(df_animals$x[i], df_animals$y[i], df_animals$angle[i], df_animals$vel[i], Param.dt)
            df_animals$x[i] <<- ns[1]
            df_animals$y[i] <<- ns[2]
            df_animals$angle[i] <<- ns[3]
            trace_df <<- rbind(trace_df, as.data.frame(list(bm=df_animals$bm[i], seq=df_animals$seq[i], t = t, x = ns[1], y = ns[2])))
        }
        else
        {
            seq <- df_animals$seq[i]
            masterseq <- df_lunchtime[df_lunchtime$moth_seq == seq, 'bat_seq']
            masterpos <- df_animals[df_animals$bm == 'Bat' & df_animals$seq == masterseq,]
            trace_df <<- rbind(trace_df, as.data.frame(list(bm=df_animals$bm[i], seq=df_animals$seq[i], t = t, x = masterpos$x, y = masterpos$y))) #, angle = masterpos$angle , vel = masterpos$vel))
        }
    }
    moth_df <- df_animals[df_animals$bm == 'Moth', ]
    bat_df <- df_animals[df_animals$bm == 'Bat',]
    for (i in (1:nrow(moth_df)))
    {
        mothx <-  moth_df$x[i]
        mothy <-  moth_df$y[i]
        mothseq <- moth_df$seq[i]
        for (j in (1:nrow(bat_df)))
        {
            batx <-  bat_df$x[j]
            baty <-  bat_df$y[j]
            batseq <- bat_df$seq[j]
            batangle <- bat_df$angle[j]
            moth_detected <- WithinRange(batx, baty, mothx, mothy, Param.BatRangeDist, batangle, Param.BatRangeAngle)
            if (moth_detected) # bat detects moths, turns towards
            {
                alpha <- calcAngle(batx, baty, mothx, mothy)
                df_animals[df_animals$bm=='Bat' & df_animals$seq == batseq, "angle"] <<- alpha
            }
            dist <- dist(mothx, mothy, batx, baty)
            if (dist <= Param.DangerZone) # moth gets eaten
            {
                BatCatchesMoth (bat_df$seq[j], mothseq, t) 
            }
            else if (dist < Param.MothRange) # moth detects bat, turns back
            {
                alpha <- calcAngle(batx, baty, mothx, mothy)
                df_animals[df_animals$bm=='Moth' & df_animals$seq == mothseq, "angle"] <<- alpha
            }
        }  
    }
}