## makeCacheMatrix returns a matrix with getter and setter methods and a private inverse variable
## cacheSolve solves the inverse of a matrix returned by makeCacheMatrix and caches the inverse variable in the passed matrix

## Returns a matrix with getter and setter methods

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  getInverse <- function() inverse
  
  setInverse <- function(newInverse) inverse <<- newInverse
  
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## Returns and caches the inverse of the given matrix made from makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message('getting cached inverse')
    inverse
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
