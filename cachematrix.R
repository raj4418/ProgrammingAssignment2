## The function
## calculates inverse of matrix and cache

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function
## to set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
  
  setmatrix <- function(y) { #set the value of the matrix
    x <<- y ## caches the inputted matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
    m <<- NULL # # sets the value of m (the matrix inverse if used cacheSolve) to NULL
  }
  getmatrix<-function() x
  setinverse<-function(solve) m <<- solve
  getinverse<-function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,setinverse = setinverse,getinverse = getinverse)
}


## calculates the inverse of the special "matrix" created with the above function
##However, it first checks to see if the inverse has already been calculated
##If so, it gets the inverse from the cache and skips the computation
##Otherwise, it calculates the inverse of the data
##and sets the value of the inverse in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
       
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}