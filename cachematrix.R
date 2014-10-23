## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
