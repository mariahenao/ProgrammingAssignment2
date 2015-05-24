
## This function creates a special Matrix to get and set the matrix value and to get and set the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv 
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Solves (calculate inverse matrix) of a matrix but first it checks to see if the matrix has already been calculated, if so
## it skips calculation and takes the value from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
