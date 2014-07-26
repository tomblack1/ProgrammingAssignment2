## Put comments here that give an overall description of what your
## functions do
##
## Overall:  Functions load matrix and solve for inverse if not 
## already set in session cache, otherwise it solves for inverse
## and sets in cache.

## Write a short comment describing this function
##
## Function takes a matrix and essentially builds a class
## with get, set, getinverse and setinverse functions to 
## reset and read from a cached copy of already solved 
## matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL
  set <- function(y){
      x <<-y
      mx <<-NULL
  }
  get <- function() x
  setinverse <- function(solve) mx <<- solve
  getinverse <- function() mx
  
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
##
## Function checks the cache for the inverse already being set
## If inverse not already set, then solves for inverse and sets 
## it into the cache for possible use later.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mx <- x$getinverse()
    if(!is.null(mx)){
      message("getting cached data")
      return(mx)
    }
    data <- x$get()
    mx <- solve(data)
    x$setinverse(mx)
    
    mx
}
