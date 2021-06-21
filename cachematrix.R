#Create the base functions for the makeCacheMatrix
#This function will create 2 pairs get/set functions to be called, 
#which check the existance of or create either a matrix or it's inverted matrix

makeCacheMatrix <- function(x = matrix()){
  #temporary 
  inv <- NULL
  
  #set the matrix informed in the function
  setMtx <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  #get the pre-existing matrix
  getMtx <- function() x
  
  #set the inverse matrix
  setInv <- function(invers) inv <<- invers
  
  #get the previous inverse matrix
  getInv <- function() inv
  list(setMtx = setMtx, getMtx = getMtx,
       setInv = setInv, getInv = getInv)
}


## Check if there's already an inverted matrix, if not create a new one

cacheSolve <- function(z, ...){
  #get the inverse matrix and check if is already created, if it is return the inverse
  invC <- z$getInv()
  
  if(!is.null(invC)){
    message("Get Inverse Matrix Data")
    return(invC)
  }
  
  #Get the matrix, invert ir and set the inverse in the cache
  data <- z$getMtx()
  invC <- solve(data)
  z$setInv(invC)
  invC
}


#Matrix Test
t <- matrix(c(-3, 5, 1, 0), nrow = 2)
t
matrixTest <- makeCacheMatrix(t)
matrixTest
cacheSolve(matrixTest)


