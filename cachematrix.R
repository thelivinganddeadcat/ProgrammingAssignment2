## The functions will allow to calculate the inverse of a matrix only once, 
## storing the inverse in cache and retrieving from it 

## makeCacheMatrix is a function creating a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}



## this is how to test the function

## create a cache matrix z
## z<-makeCacheMatrix(matrix(3:6,nrow=2,ncol=2))
## check it with z$get()
## calculate the inverse with CacheSolve(z)
## rerun CacheSolve(z) and verify that it prints the inverse with the additonal 
## message "getting cached data"
