## The following two functions together cache the inverse of a matrix.  The makeCacheMatrix function is used to
## create an object that stores a matrix and its inverse.  The cacheSolve function requires an input argument
## of type makeCacheMatrix and returns the inverse matrix that is stored in the makeCacheMatrix object's environment
## or calculates the inverse of a new matrix.

## Create a list object that stores a matrix and its inverse.  Also includes getter and setter methods to retrieve
## the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # the object m holds the inverse of a matrix.  
  m <- NULL
 
  ## This function assigns the input argument to the x object and assigns the value of null to the m object
  ## to clear any value of m that had been cached by a prior execution of cacheInverse.
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  ## This function caches the inverse of a matrix computed in the cacheSolve function
  setInverse <- function(inv) m <<- inv
  
  getInverse <- function() m
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Compute the inverse of the matrix returned by a makeCacheMatrix object. 
## If the inverse has already been calculated and the matrix has not changed, 
## then the inverse of the matrix is retrieved from cache.
cacheSolve <- function(x, ...) {
  
  # Retrieve the current inverse matrix from cache.  If not null, return the inverse matrix.
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # The current inverse matrix is null, so retrieve the matrix and calculate its inverse.
  data <- x$get()
  m <- solve(data, ...)
  # cache the matrix inverse
  x$setInverse(m)
  m
}
