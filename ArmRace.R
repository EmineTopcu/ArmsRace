library(ggplot2)
library(htmlwidgets)
library(plotly)

source("Const.R")

df_animals <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(df_animals) <- c('bm', 'seq', 'x', 'y', 'angle', 'vel')

trace_df <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(trace_df) <- c('bm', 'seq', 'time', 'x', 'y') #, 'angle', 'vel')
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
  deltax <- vel * dt *  cos(angle * pi / 180)
  deltay <- vel * dt *  sin(angle * pi / 180)
  turn <- FALSE
  quartile <- replicate(4 , 1)
  if (x + deltax < 0)
  {
    quartile[2] <- 0
    quartile[3] <- 0
  }
  else if (x + deltax > width)
  {
    quartile[1] <- 0
    quartile[4] <- 0
  }
  if (y + deltay < 0)
  {
    quartile[3] <- 0
    quartile[4] <- 0
  }
  else if (y + deltay > height)
  {
    quartile[1] <- 0
    quartile[2] <- 0
  }
  if (sum(quartile) < 4)
  {
    direction <- which(quartile == 1)
    minangle <- (direction[1] - 1) * 90
    if (sum(quartile) == 1)
      maxangle <- minangle + 90
    else
      maxangle <- (direction[-1]) * 90
    if (minangle == 0 & maxangle == 360)
    { minangle <- -90; maxangle <- 90 }
    angle<- runif(1, minangle, maxangle) %% 360
    return (nextstep(x, y, angle, vel, dt))
  }
  x <- x + deltax
  y <- y + deltay
  return (c(x, y, angle))
}


dist <- function(x1, y1, x2, y2)
{
  return (sqrt((x1-x2)^2 + (y1-y2)^2))
}

calcAngle <- function(x1, y1, x2, y2)
{
  if (x1 == x2)
  {
    if (x2 > x1) 
      return (0)
    else 
      return (180)
  }
  alpha <- atan((y2 - y1) / (x2 - x1)) * 180 / pi
  if (x2 < x1)
    alpha <- alpha + 180
  return (alpha)
}

withinRange <- function(x1, y1, x2, y2, range, alpha, beta)
{
  if (dist(x1,y1,x2,y2) > range)
    return(FALSE)
  
  
    
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
      trace_df <<- rbind(trace_df, as.data.frame(list(bm=df_animals$bm[i], seq=df_animals$seq[i], t = t, x = masterpos$x, y = masterpos$y))) #, angle = masterpos$angle , vel = masterpos$vel))
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
      batx <-  bat_df$x[j]
      baty <-  bat_df$y[j]
      batseq <- bat_df$seq[j]
      dist <- dist(mothx, mothy, batx, baty)
      if (dist < batrange) # bat detects moths, turns towards
      {
        alpha <- calcAngle(batx, baty, mothx, mothy)
        df_animals[df_animals$bm=='b' & df_animals$seq == batseq, "angle"] <<- alpha
      }
      if (dist < dangerzone)
      {
        masterofmoths[mothseq] <<-  bat_df$seq[j]
        df_animals[df_animals$bm=='m' & df_animals$seq == mothseq, "x"] <<- -width
        df_animals[df_animals$bm=='m' & df_animals$seq == mothseq, "y"] <<- -height
        df_animals[df_animals$bm=='m' & df_animals$seq == mothseq, "vel"] <<- 0
      }
    }  
  }
}


for (i in (1:100))
{
  timeincr(i)
}



p <- ggplot(trace_df, aes(x, y, frame = t)) +
 xlim(0, width) + ylim(0, height) + theme_bw() +
  geom_point(aes(size = bm, shape = bm, color = bm, frame = t, ids = paste(bm, seq))) +
  scale_size_manual(values=c(5, 2)) +
  scale_shape_manual(values=c(11, 5)) +
  scale_color_manual(values=c("black", "navajowhite4"))



pp <- ggplotly(p) %>% animation_opts(500, easing = "linear", redraw = FALSE)
pp
saveWidget(pp, file = "animation.html", selfcontained = TRUE)
