## makeCacheMatrix creates a special matrix which is really a list containing a function
## this is used by cacheSolve.R to either make or retieve the inverse of a vector from cache.

makeCacheMatrix <- function(x = matrix()) {
  # creates and sets the intitial inverse value to NULL
  inverse1 <- NULL
  
  #creates the matrix in the working environment
  set <- function(y) {
    x <<- y
    inverse1 <<- NULL
  }
  
  # retrieves the matrix value 
  get <- function() x
  
  # creates the inverted matrix and saves in cache
  setinverse <- function(inverse2) inverse1 <<- inverse2
  
  # retrieves the inverse matrix from cache
  getinverse <- function() inverse1
  
  # makes a list of the functions in the working environment
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## works with makeCacheMatrix.R
## If the inverse matrix exists, it retrieves it from cache
## If the inverse does not exist, it creates it

cacheSolve <- function(x, ...) {
  
  # get the inerse value from cache (it may be NULL)
  inverse1 <- x$getinverse()
  
  # if the matrix inverse in cache is not NULL, return the value from cache
  if(!is.null(inverse1)) {
    message("getting cached data.")
    return(inverse1)
  }
  
  # if the matrix inerse in cache is NULL, solve the matrix
  # and set the value in cache
  else {
  originalmatrix <- x$get()
  inverse1 <- solve(originalmatrix)
  x$setinverse(inverse1)
  inverse1
  }
}
