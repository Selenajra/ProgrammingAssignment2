## These functions implement storage of a matrix and it's inverse to remove the need
## to re-calculate the matrix inverse every time it is needed.


## This function stores a matrix and a cached value intended to hold the calculated
## inverse of the matrix.
## Get & set functions access the matrix value
## setCache and getCache functions access the variable intended to hold the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## initialize the "cache" variable to NULL on instantiation
  
  ## stores the passed matrix in the "x" variable, and resets m to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## prints out the current value stored in "x"
  get <- function() x
  
  ## sets the value in "m" to the passed value
  setCache <- function(solve) m <<- solve
  
  ## prints the current value stored in "m"
  getCache <- function() m
  
  ## list of functions to allow access
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}

## This function checks to see if the passed makeCacheMatrix has a cached inverse
## if so that value is printed.  If not the inverse of the matrix stored the the
## makeCacheMatrix is calculated, cached, and then printed.
cacheSolve <- function(x, ...) {     
  m <- x$getCache()  ## get the current cache value from "x" and store it as "m"
  
  ## if "m" is not null then return the value of "m"
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if "m" is null get the matrix currently stored in "x"
  data <- x$get()
  
  ## calculate the inverse and store it in "m"
  m <- solve(data, ...)
  
  ## set the cache value in "x" to the solved inverse
  x$setCache(m)
  
  ## print the value of the solved inverse
  m
}
