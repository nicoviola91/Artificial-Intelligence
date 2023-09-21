library("DeliveryMan")

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # What is our goal?
  if(carInfo$load == 0) {
    # Set goal to pick up nearest package
    carInfo$mem$goal <- nextPickup(trafficMatrix, carInfo, packageMatrix)
  } else {
    # Carry package to its destination
    carInfo$mem$goal <- packageMatrix[carInfo$load, c(3,4)]
  }
  
  # How do we get there?
  path <- aStar(trafficMatrix, carInfo,packageMatrix)
  carInfo$nextMove <- nextMove(path, carInfo$mem$goal[1], carInfo$mem$goal[2])
  
  return(carInfo)
}

# Find the nearest pickup location for an undelivered package
nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  distanceVector = abs(packageMatrix[,1] - carInfo$x) + abs(packageMatrix[,2] - carInfo$y)
  distanceVector[packageMatrix[,5] != 0] = Inf # If package destination and location overlap
  return(packageMatrix[which.min(distanceVector), c(1,2)])
}

# Returns next move based on path and destination
nextMove <- function(path, destX, destY) {
  
  if (length(path) == 0) {
    # Need to wait for new goal from myFunction and subsequently new path from A*
    return(5)
  } 
  
  # Get current x and y
  x = path[[1]][[1]]
  y = path[[1]][[2]]
  
  # If next step is not the goal, set destination to next step in the path
  if (length(path) > 1) {
    destX = path[[2]][[1]]
    destY = path[[2]][[2]]
  } 
  
  if(x < destX) {
    return(6)
  } else if (x > destX) {
    return(4)
  } else if (y < destY) {
    return(8)
  } else if (y > destY) {
    return(2)
  }
  
}

aStar <- function(trafficMatrix, carInfo, packageMatrix) {
  
  # Initial node. Node structure: [x, y, cost, path]
  init <- list(carInfo$x, carInfo$y, 0, list())
  
  # Initialize frontier
  frontier <- list(init)
  
  # Save destination coordinates
  destX <- carInfo$mem$goal[1]
  destY <- carInfo$mem$goal[2]
  
  # Initialize visited set
  visited <- list(init)
  
  repeat {
    head <- frontier[[1]] # Get first node from frontier
    frontier <- frontier[-1] # Pop first node from frontier
    
    # When A* has found a path, return it
    if (head[[1]] == destX & head[[2]] == destY) {
      return(head[[4]])
    } 
    
    # Add head to path
    path <- head[[4]]
    path[[length(path)+1]] <- list(head[[1]], head[[2]]) 
    
    # Get neighbors of frontier node
    neighbors <- getNeighbors(head[[1]], head[[2]], head[[3]], destX, destY, trafficMatrix)
    
    # Add unvisited neighbors to frontier
    for (n in neighbors) {
      vis = FALSE
      for (v in visited) {
        if (v[[1]] == n[[1]] & v[[2]] == n[[2]]) {
          vis = TRUE
        }
      }
      if (vis == FALSE) {
        newNode <- list(n[[1]], n[[2]], n[[3]], path)
        frontier[[length(frontier)+1]] <- newNode
        visited[[length(visited)+1]] <- newNode
      }
      
    }
    # Sort frontier based on costs
    frontier <- frontier[order(sapply(frontier,'[[',3))]
  }
  
}

# Function for getting neighbor nodes and their costs (f = g + h)
getNeighbors <- function(x, y, cost, destX, destY, trafficMatrix) {
  
  # Get traffic dimensions for checking bounds
  hEdges <- dim(trafficMatrix$hroads)[1]
  vEdges <- dim(trafficMatrix$vroads)[2]
  
  # List for storing neighbors
  neighbors <- list()
  
  # Right neighbor
  if (x <= hEdges) {
    neighbors[[length(neighbors)+1]] <- list(x+1, y, cost + trafficMatrix$hroads[x, y] + abs(destX-(x+1)) + abs(destY-y))
  }
  
  # Left neighbor
  if (x-1 >= 1) {
    neighbors[[length(neighbors)+1]] <- list(x-1, y, cost + trafficMatrix$hroads[x-1, y] + abs(destX-(x-1)) + abs(destY-y))
  }
  
  # Up neighbor
  if (y <= vEdges) {
    neighbors[[length(neighbors)+1]] <- list(x, y+1, cost + trafficMatrix$vroads[x, y] + abs(destX-x) + abs(destY-(y+1)))
  }
  
  # Down neighbor
  if (y-1 >= 1) {
    neighbors[[length(neighbors)+1]] <- list(x, y-1, cost + trafficMatrix$vroads[x, y-1] + abs(destX-x) + abs(destY-(y-1)))
  } 
  return(neighbors)
  
}

runDeliveryMan(carReady = myFunction, dim = 10, turns = 2000, doPlot = T, pause = 0.1, del = 5, verbose = T)

testDM(myFunction, verbose = 0, returnVec = FALSE, n = 500, seed = 21, timeLimit = 250)