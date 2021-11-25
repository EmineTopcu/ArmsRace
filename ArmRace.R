library(ggplot2)
library(htmlwidgets)
library(plotly)

source("Const.R")
source("RangeFunctions.R")

# escape begavior - by default
# startle
# learn from startle
# duration= 2 time how long it takes for moth to cross diagonally
# repeat 100 times
# percentage of capture, time till capture, for both bath and moth

height <- 150
width <- 150
timerange <- 50
dt <- 0.1


dangerzone <- 1
batrange_dist <- 15
batrange_angle <- 150
mothrange <- 100

numofbats <- 3
velbat <- 10 # 3-15
velbatsd <- 2
numofmoths <- 10
velmoth <- 5


df_animals <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(df_animals) <- c('bm', 'seq', 'x', 'y', 'angle', 'vel')

df_lunchtime <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df_animals) <- c('moth_seq', 'bat_seq', 'eaten_at')


trace_df <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(trace_df) <- c('bm', 'seq', 'time', 'x', 'y') #, 'angle', 'vel')

initializeAnimals <- function(num, vel, bm)
{
    y <- runif(num, 1, height)
    x <- runif(num, 1, width)
    angle <- runif(num,0, 360)
    vel <- rep(vel, num)
    df_animals <<- rbind(df_animals, (as.data.frame(list(bm=rep(bm, num), seq=(1:num), x = x, y = y, angle = angle, vel = vel))))
}

initializeAnimals(numofbats, velbat, 'Bat')
initializeAnimals(numofmoths, velmoth, 'Moth')

BatCatchesMoth <- function(batseq, mothseq, t)
{
    df_lunchtime <<-  rbind(df_lunchtime, (as.data.frame(list(moth_seq=mothseq, bat_seq=batseq, eaten_at = t))))
    
    df_animals[df_animals$bm=='Moth' & df_animals$seq == mothseq, "x"] <<- -width
    df_animals[df_animals$bm=='Moth' & df_animals$seq == mothseq, "y"] <<- -height
    df_animals[df_animals$bm=='Moth' & df_animals$seq == mothseq, "vel"] <<- 0
}

timeincr <- function(t)
{
    for (i in (1:nrow(df_animals)))
    {
        if (df_animals$vel[i] > 0)
        {
            ns <- nextstep(df_animals$x[i], df_animals$y[i], df_animals$angle[i], df_animals$vel[i], dt)
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
            moth_detected <- withinRange(batx, baty, mothx, mothy, batrange_dist, batangle, batrange_angle)
            if (moth_detected) # bat detects moths, turns towards
            {
                alpha <- calcAngle(batx, baty, mothx, mothy)
                df_animals[df_animals$bm=='Bat' & df_animals$seq == batseq, "angle"] <<- alpha
            }
            dist <- dist(mothx, mothy, batx, baty)
            if (dist < dangerzone)
            {
                BatCatchesMoth (bat_df$seq[j], mothseq, t) 
            }
            else if (dist < mothrange)
            {
                alpha <- calcAngle(batx, baty, mothx, mothy)
                df_animals[df_animals$bm=='Moth' & df_animals$seq == mothseq, "angle"] <<- alpha
            }
        }  
    }
}

Simulate <- function()
{
    stats <- c('Victim' = 0, 'Prey' = 0, 'HuntTime' = 0)
    for (i in seq(1, timerange, dt))
    {
        timeincr(i)
    }
    stats["Victim"] <- nrow(df_lunchtime) / numofmoths
    stats["Prey"] <- nrow(df_lunchtime) / numofbats
    stats["HuntTime"] <- mean(df_lunchtime$eaten_at)
    return (stats)
}

stats <- Simulate()

p <- ggplot(trace_df, aes(x, y, frame = t)) +
 xlim(0, width) + ylim(0, height) + theme_bw() +
  geom_point(aes(size = bm, shape = bm, color = bm, frame = t, ids = paste(bm, seq))) +
  scale_size_manual(values=c(5, 2)) +
  scale_shape_manual(values=c(11, 5)) +
  scale_color_manual(values=c("black", "navajowhite4"))



pp <- ggplotly(p) %>% animation_opts(500, easing = "linear", redraw = FALSE)
pp
# saveWidget(pp, file = "animation.html", selfcontained = TRUE)

dd <- function(a)
{
    return (a**2)
}

dd(c(5,7))   
