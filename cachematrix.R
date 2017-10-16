## This file contatins two functions. 
## The first function creates an R object that stores a matrix and its inverse
## The second function calls on the first function for the inverse, and if it is null, calculates the inverse

## This function creates four functions and two data objects

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calls on the inverse stored from makeCacheMatrix.
## If the inverse is found from makeCacheMatrix, the message will be printed and the inverse returned
## If the inverse is not found from makeCacheMatrix it will perform solve and print the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
