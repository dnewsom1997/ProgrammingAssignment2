## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL                      ## Holding the value of matrix inverse
  set <- function(y){       
    x <<- y                      ## Value of matrix from other environment
    j <<- NULL                   ## Reset inv to NULL
  }
  get <- function()x             ## WIll return value of matrix argument
  setInverse <- function(inverse)j <<- inverse   ## Assigns value of inv to the other environment
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)                  ## Needed for functions with "$ operator"
}


## Write a short comment describing this function
## Computing the inverse of the matrix will be returned with makeCacheMatrix
## The matrix will not change given the inverse has already been calculated
## cachSolve will take the inverse from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}