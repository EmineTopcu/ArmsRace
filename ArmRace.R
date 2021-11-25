library(ggplot2)
library(htmlwidgets)
library(plotly)

source("RangeFunctions.R")
source("MainFunctions.R")

# escape begavior - by default
# startle
# learn from startle
# duration= 2 time how long it takes for moth to cross diagonally
# repeat 100 times
# percentage of capture, time till capture, for both bath and moth

Simulate <- function()
{
    df_animals <<- data.frame(matrix(ncol = 6, nrow = 0))
    colnames(df_animals) <<- c('bm', 'seq', 'x', 'y', 'angle', 'vel')
    
    df_lunchtime <<- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_animals) <<- c('moth_seq', 'bat_seq', 'eaten_at')
    
    trace_df <<- data.frame(matrix(ncol = 5, nrow = 0))
    colnames(trace_df) <<- c('bm', 'seq', 'time', 'x', 'y') #, 'angle', 'vel')
    
    InitializeAnimals(Param.NumOfBats, Param.VelBat, Param.VelBatSD, 'Bat')
    InitializeAnimals(Param.NumOfMoths, Param.VelMoth, Param.VelMothSD, 'Moth')
    
    timerange <- (Param.Height**2 + Param.Width**2)^0.5 / Param.VelMoth * 2
    stats <- c('Victim' = 0, 'Prey' = 0, 'HuntTime' = 0)
    for (i in seq(1, timerange, Param.dt))
    {
        TimeIncr(i)
    }
    stats["Victim"] <- nrow(df_lunchtime) / Param.NumOfMoths
    stats["Prey"] <- nrow(df_lunchtime) / Param.NumOfBats
    stats["HuntTime"] <- mean(df_lunchtime$eaten_at) / timerange
    return (stats)
}

Animate <- function(fileName = NULL)
{
    p <- ggplot(trace_df, aes(x, y, frame = t)) +
        xlim(0, Param.Width) + ylim(0, Param.Height) + theme_bw() +
        geom_point(aes(size = bm, shape = bm, color = bm, frame = t, ids = paste(bm, seq))) +
        scale_size_manual(values=c(5, 2)) +
        scale_shape_manual(values=c(11, 5)) +
        scale_color_manual(values=c("black", "navajowhite4"))

    pp <- ggplotly(p) %>% animation_opts(500, easing = "linear", redraw = FALSE)
    pp
    if (! is.null(fileName))
        saveWidget(pp, file = fileName, selfcontained = TRUE)
}

RandomHunt <- function(numOfRuns)
{
    InitializeParameters()
    mothrange <<- 0 # moth cannot detect bats
    batrange_dist <<- 1000 # bat cannot detect moths
    stats <- matrix(numeric(numOfRuns * 3), nrow = numOfRuns)
    colnames(stats) <- c('Victim', 'Prey', 'HuntTime')
    for (i in (1:numOfRuns))
    {
        stats[i, ] <- Simulate()        
    }
    hist(stats[,'Victim'])
    hist(stats[,'Prey'])
    hist(stats[,'HuntTime'])
}

RandomHunt(30)
Animate("RandomHunt.html")


