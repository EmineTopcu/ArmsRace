# Calculates the next position of the animal based on the current position and velocity
# If the border of the area is reached, it turns back at a random angle
NextStep <- function(x, y, angle, vel, dt)
{
    deltax <- vel * dt *  cos(angle * pi / 180)
    deltay <- vel * dt *  sin(angle * pi / 180)
    quartile <- replicate(4 , 1)
    if (x + deltax < 0)
    {
        quartile[2] <- 0
        quartile[3] <- 0
    }
    else if (x + deltax > Param.Width)
    {
        quartile[1] <- 0
        quartile[4] <- 0
    }
    if (y + deltay < 0)
    {
        quartile[3] <- 0
        quartile[4] <- 0
    }
    else if (y + deltay > Param.Height)
    {
        quartile[1] <- 0
        quartile[2] <- 0
    }
    if (sum(quartile) < 4)
    {
        direction <- which(quartile == 1)
        minangle <- (direction[1] - 1) * 90
        if (sum(quartile) == 1) maxangle <- minangle + 90
        else maxangle <- (direction[-1]) * 90
        if (minangle == 0 & maxangle == 360)
        { minangle <- -90; maxangle <- 90 }
        angle<- runif(1, minangle, maxangle) %% 360
        return (NextStep(x, y, angle, vel, dt))
    }
    x <- x + deltax
    y <- y + deltay
    return (c(x, y, angle))
}

# Calculate the Euler distance between two points
CalcDist <- function(x1, y1, x2, y2)
{
    return (sqrt((x1-x2)^2 + (y1-y2)^2))
}

# Calculate the angle to go from point 1 (x1, y1) to point 2 (x2, y2)
CalcAngle <- function(x1, y1, x2, y2)
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

# Calculates whether point 2 (x2, y2) is within range for point 1 (x1, y1)
# point 1 has the direction of alpha, and swiping range of beta from the central axis of direction
WithinRange <- function(x1, y1, x2, y2, range, alpha, beta)
{
    if (CalcDist(x1, y1, x2, y2) > range)
        return(FALSE)
    gamma <- CalcAngle(x1, y1, x2, y2)
    diff <- abs(gamma - alpha)
    if (diff > 180) diff <- diff - 180
    return (diff <= beta/2)
}
