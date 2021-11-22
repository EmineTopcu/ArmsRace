library(ggplot2)
library(plotly)


height <- 1000
width <- 2000
dangerzone <- 20

numofbats <- 2
velbat <- 20
numofmoths <- 4
velmoth <- 5

Area <- matrix(nrow = height, ncol = width)

df_animals <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(df_animals) <- c('bm', 'seq', 'x', 'y', 'angle', 'vel')

trace_df <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(trace_df) <- c('bm', 'seq', 'time', 'x', 'y')
masterofmoths <- rep(0, numofmoths)

initializeAnimals <- function(num, vel, bm)
{
  y <- runif(num, 1, height)
  x <- runif(num, 1, width)
  angle <- runif(num,0, 360)
  vel <- rep(vel, num)
  df_animals <<- rbind(df_animals, (as.data.frame(list(bm=rep(bm, num), seq=(1:num), x = x, y = y, angle = angle, vel = vel))))
}

initializeAnimals(numofbats, velbat, 'b')
initializeAnimals(numofmoths, velmoth, 'm')

nextstep <- function(x, y, angle, vel, dt){
  deltax <- vel * dt *  cos(angle)
  deltay <- vel * dt *  sin(angle)
  turn <- FALSE
  minangle <- 0
  maxangle <- 360
  if (x + deltax < 0)
  {
    minangle <- -90
    maxangle <- 90
    turn <- TRUE
  }
  else if (x + deltax > width)
  {
    minangle <- 90
    maxangle <- 270
    turn <- TRUE
  }
  if (y + deltay <0)
  {
    minangle <- max(180, minangle)
    maxangle <- min(360, maxangle)
    turn <- TRUE
  }
  else if (y + deltay > height)
  {
    minangle <- max(0, minangle)
    maxangle <- min(180, maxangle)
    turn <- TRUE
  }
  if (turn)
  {
    if (minangle>maxangle)
    {
      temp <- minangle+180
      minangle <- maxangle+180
      maxangle <- temp
    }
    angle<- runif(1, minangle, maxangle) %% 360
    deltax <- 0
    deltay <- 0
  }  
  x <- x + deltax
  y <- y + deltay
  return (c(x, y, angle))
}


dist <- function(x1, y1, x2, y2)
{
  return (sqrt((x1-x2)^2 + (y1-y2)^2))
}

timeincr <- function(t)
{
  for (i in (1:nrow(df_animals)))
  {
    if (df_animals$vel[i] > 0)
    {
      ns <- nextstep(df_animals$x[i], df_animals$y[i], df_animals$angle[i], df_animals$vel[i], 1)
      df_animals$x[i] <<- ns[1]
      df_animals$y[i] <<- ns[2]
      df_animals$angle[i] <<- ns[3]
      trace_df <<- rbind(trace_df, as.data.frame(list(bm=df_animals$bm[i], seq=df_animals$seq[i], t = t, x = ns[1], y = ns[2])))
    }
    else
    {
      seq <- df_animals$seq[i]
      masterseq <- masterofmoths[seq]
      masterpos <- df_animals[df_animals$bm == 'b' & df_animals$seq == masterseq,]
      trace_df <<- rbind(trace_df, as.data.frame(list(bm=df_animals$bm[i], seq=df_animals$seq[i], t = t, x = masterpos$x, y = masterpos$y)))
    }
  }
  moth_df <- df_animals[df_animals$bm == 'm', ]
  bat_df <- df_animals[df_animals$bm == 'b',]
  for (i in (1:nrow(moth_df)))
  {
    mothx <-  moth_df$x[i]
    mothy <-  moth_df$y[i]
    mothseq <- moth_df$seq[i]
    for (j in (1:nrow(bat_df)))
    {
      batx = bat_df$x[j]
      baty = bat_df$y[j]
      if (dist(mothx, mothy, batx, baty) < dangerzone)
      {
        masterofmoths[mothseq] <<-  bat_df$seq
        df_animals[df_animals$bm=='m' & df_animals$seq == mothseq, "x"] <<- 0
        df_animals[df_animals$bm=='m' & df_animals$seq == mothseq, "y"] <<- 0
        df_animals[df_animals$bm=='m' & df_animals$seq == mothseq, "vel"] <<- 0
      }
    }  
  }
}


for (i in (1:300))
{
  timeincr(i)
}



p <- ggplot(trace_df, aes(x, y)) +
 xlim(0, width) + ylim(0, height) + theme_bw() +
  geom_point(aes(size = bm, shape = bm, color = bm, frame = t, ids = paste(bm, seq))) +
  scale_size_manual(values=c(5, 2)) +
  scale_shape_manual(values=c(11, 5)) +
  scale_color_manual(values=c("black", "navajowhite4"))


ggplotly(p)
