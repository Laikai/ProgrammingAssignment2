## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  setMtx <- function(y){
    x <<- y
    inv <<- NULL
  }
  getMtx <- function() x
  setInv <- function(invers) inv <<- invers
  getInv <- function() inv
  list(setMtx = setMtx, getMtx = getMtx,
       setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(z, ...){
  invC <- z$getInv()
  if(!is.null(invC)){
    message("Get Inverse Matrix Data")
    return(invC)
  }
  data <- z$getMtx()
  invC <- solve(data)
  z$setInv(invC)
  invC
}
