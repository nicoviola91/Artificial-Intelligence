# File:         demo.r 
# Description:  Naive demo-solution given at classroom session
#               for Project 1, Artificial Intelligence 2019, UU
# Author:       Fredrik Nilsson
# Modified by:  Marcello Vendruscolo (2021)
# Modified by:  Nicolas Viola (2023)

# Install the package
# install.packages("DeliveryMan_1.1.0.tar.gz", repos = NULL, type="source")

# Load the library
library("DeliveryMan")

# Read documentation
# ?runDeliveryMan
# ?testDM

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # What is our goal?
  if(carInfo$load == 0) {
      #if no load, then search for the nearest package to pick up
      carInfo$mem$goal <- nextPickup(trafficMatrix, 
                                     carInfo, 
                                     packageMatrix)
    } else {
      #else, get package destination as goal
      carInfo$mem$goal <- packageMatrix[carInfo$load, c(3,4)]
  }
  
  # How do we get there?
  carInfo$nextMove <- nextMove(trafficMatrix,
                               carInfo,
                               packageMatrix)
  return(carInfo)
}

# Find the nearest pickup location for an undelivered package
nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  distanceVector = abs(packageMatrix[,1] - carInfo$x) + abs(packageMatrix[,2] - carInfo$y)
  distanceVector[packageMatrix[,5] != 0] = Inf
  return(packageMatrix[which.min(distanceVector), c(1,2)])
}

# Calculate heuristic h(x) from given node to goal
heuristic <- function(carX, carY, goal) {
  distance = abs(goal[1] - carX) + abs(goal[2] - carY) # Manhattan distance from car position to goal
  return(distance)
}


# Find the move to get to carInfo$mem$goal
nextMove <- function(trafficMatrix, carInfo, packageMatrix) {
  #Apply A* from origin to destination
  path <- astar(trafficMatrix,carInfo)
  #From solution, pick first node on path and move on its direction
  chosenNextNode <- path[[1]]
  if(carInfo$x > chosenNextNode$x && carInfo$y == chosenNextNode$y ){
    return(4) #move Left
  }
  else if(carInfo$x == chosenNextNode$x && carInfo$y > chosenNextNode$y ){
    return(2) #move Down
  }
  else if(carInfo$x < chosenNextNode$x && carInfo$y == chosenNextNode$y ){
    return(6) #move Right
  }
  else if(carInfo$x == chosenNextNode$x && carInfo$y < chosenNextNode$y ){
    return(8) #move Up
  }
  else if(carInfo$x == chosenNextNode$x && carInfo$y == chosenNextNode$y ){
    return(5) #stay
    #would like to log the Stay action too
  }
  else{
    #dont know  how ot log, but this is for a validation that the result is only one move away
    #it should only move to a surrounding place, if not, the algorithm is wrong
  }

}

###################################################################################
#                                                                                 #
#                             HERE I BEGAN MY INTERVENTION                        #
#                                                                                 #
###################################################################################

#Node structure that i am planning on using
# node <- list(
#   x<-10,
#   y<-5,
#   cost <- 0,
#   path <- list(1,3))

#I am not sure about this, if the costs are stored as coordinate (from,to) in the respective matrices vroads and hroads
#Assuming that, i check the cost of moving from x1 to x2 or y1 to y2 depending on the direction of the movement
gFunction<- function(trafficMatrix,origin,goal){
  if(origin$x==goal[1]){ #if x coordinate is the same, the movement was vertical
    return(trafficMatrix$vroads[origin$y,goal[2]])
  }
  else{ #if y coordinate is the same, the movement was horizontal
    return(trafficMatrix$hroads[origin$x,goal[1]]])
  }
  
}

#Definition of f Function as h + g
fFunction<- function(trafficMatrix,origin,goal){
  return(heuristic(origin$x,origin$y,goal)+gFunction(trafficMatrix,origin,goal))
}

#useful function to pop the node from the frontier without having to keep order
#the correctness of this function depends on wether the frontier gets affected from the function (if the pointer stays the same as in Python and C)
popFrontier<- function(frontier){
    min_pos <- which.min(sapply(frontier, fFunction(trafficMatrix,node,goal))) #REVIEW since i calculate costs in TakeAction function
    node <- frontier[[min_pos]]
    if(min_pos == 1){
      frontier <- frontier[2,length(frontier)]  
    }
    else if(min_pos == length(frontier)){
     frontier <- frontier[1,length(frontier)-1]   
    }
    else{
      frontier <-c(frontier[1,min_pos-1] ,frontier[min_pos+1,length(frontier)]) #this should be checked, not sure about indices
    }
    return(node)
}

#Another side function to keep the code clear. I create the new node based on the action
takeAction <- function(action,trafficMatrix,currentNode){
  maxDimV <- dim(trafficMatrix$vroads) #to control not to pass the borders
  maxDimH <- dim(trafficMatrix$hroads) #to control not to pass the borders
  if(action==2){ #Down
    new_y <- max(currentNode$y-1,1)
    nextNode <- list(x<-currentNode$x,y<-new_y,cost<- currentNode$cost + trafficMatrix$vroads[currentNode$y,new_y] ,path<-c(currentNode$path,currentNode))
  }
  else if(action==4){ #Left
    new_x <- max(currentNode$x-1,1)
    nextNode <- list(x<-new_x,y<-currentNode$y,cost<- currentNode$cost + trafficMatrix$hroads[currentNode$x,new_x] ,path<-c(currentNode$path,currentNode))
  }
  else if(action==6){ #Right
    new_x <- min(carInfo$x+1,maxDimH)
    nextNode <- list(x<-new_x,y<-currentNode$y,cost<- currentNode$cost + trafficMatrix$hroads[currentNode$x,new_x] ,path<-c(currentNode$path,currentNode))
  }
  else if(action==8){ #Up
    new_y <-min(carInfo$y+1,maxDimV)
    nextNode <- list(x<-currentNode$x,y<-new_y,cost<- currentNode$cost + trafficMatrix$vroads[currentNode$y,new_y] ,path<-c(currentNode$path,currentNode))
  }
  else{ #Stay
    nextNode <- currentNode
  }
  return(nextNode)  
}
#Equivalent to in, but with custom comparisson. Since the node might be the same but the cost and path differ, so i have to only check the coordinates
contained <- function(set1, set2){
  for(n1 in set1){
    for(n2 in set2){
      if(n1$x == n2$x && n1$y==n2$y){
        return(1)
      }
    }
  }
  return(0)
}

# Find the move to get to carInfo$mem$goal
astar <- function(trafficMatrix, carInfo) {
  node <- list(x<-carInfo$x,y<-carInfo$y,cost<-0,path<-list()) #starting node
  goal <- carInfo$mem$goal #goal[1] = x coordinate of Goal, goal[2] = y coordinate of Goal, 
  frontier <- list() # List of frontier.
  frontier <- c(frontier,node) #frontier.push(node)
  explored <- list() # List of nodes
  
  actions <- c(2, 4, 6, 8, 0) #2 down, 4 left, 6 right, 8 up, 0 stay stil

  #fix condition
  while(1){
    if(frontier is null){ 
      return(NULL) #error
    }
    #get node from frontier with least cost = frontier.pop()
    
    node <- popFrontier(frontier) #if lists pointer keep pointing to the same list in functions

    if(node$x == goal[1] && node$y==goal[2] && node in explored)  { #Is goal and explored
      return(node$path)
    }
    explored <- c(explored,node) #explored.push(node)
    for(action in actions){ #I explore the frontier
      nextNode <- takeAction(action,trafficMatrix,node)
      if(!(contained(nextNode,explored)) or !(contained(nextNode,frontier))){
        frontier <- c(frontier,nextNode)
      }
      else if(contained(nextNode,frontier)){
        #if lower cost, replace frontier node with new one:

        #find element to get cost
        #compare costs
        #replace if cost is lower
      }
    }
  }
   


}
