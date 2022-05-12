# ===========================
# defining the environment
E <- matrix(0, 10, 15)
E[4, 1:5] <- NA
E[7:10, 3] <- NA
E[3:7, 8] <- NA
E[1:3, 11] <- NA
E[5, 13:15] <- NA
image(1:15, 1:10, t(E), xlab="", ylab="")
grid(nx=15, ny=10)
# ===========================
# checking a position p to be valid in the environment E
isvalid <- function(p, E)
{
  nok <- (p[1] < 1) | (p[1] > 10) | 
    (p[2] < 1) | (p[2] > 15)
  if (!nok) 
    nok <- nok | is.na(E[p[1], p[2]])
  !nok
}
# ===========================
# checking a position p to be new in the solution path
isnew <- function(p, path)
{
  nok <- logical(length(path))
  for (i in 1:length(path))
  {
    nok[i] <- all(p == path[[i]])
  }
  return(!any(nok))
}
# ===========================
# distance between to positions p1 and p2
dist <- function(p1, p2)
{
  abs(p1[1] - p2[1]) + abs(p1[2] - p2[2])
}
# ===========================

# initial state
start <- c(10, 1)
# goal state
goal <- c(1, 15)

move <- 0
path <- list()

current <- start
E[current[1], current[2]] <- -1
# ===========================
while (any(current != goal))
{
  path[[move  + 1]] <- current
  mv <- list(c(0, 1), c(0, -1), c(1, 0), c(-1, 0))
  fvalue <- numeric(length(mv))
  for (i in 1:length(mv))
  {
    p <- current + mv[[i]]
    if (isvalid(p, E) & isnew(p, path))
    {
       h <- dist(p, goal)
       g <- move
       f <- g + h
       fvalue[i] <- f
    } else{
      fvalue[i] <- 10000 + move
    }
  }
  fmin <- min(fvalue)
  idx <- which(fvalue == fmin)
  if (length(idx) == length(mv))
    stop("can not find a solution")
  if (length(idx) == 1)
  {
    imin <- idx
  } else{
    imin <- sample(idx, 1)
  }
  current <- current + mv[[imin]]
  move <- move + 1
  E[current[1], current[2]] <- -1
  image(1:15, 1:10, t(E), xlab="", ylab="")
  grid(nx=15, ny=10)
  
  print(paste("move", move))
  print(c(fmin=fmin, imin=imin))
  print("position")
  print(current)
  
  Sys.sleep(1)
}

# ===========================
