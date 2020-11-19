## Programming Assignment 2: R Programming
## Create a pair of functions that calculate the inverse of a matrix or look up in the cache
## if the calculation for the same matrix was previously done

## First part: "special" matrix creation
## This section creates a "special" matrix that can save into the cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inversemat <- NULL
  set <- function(y){
    x <<- y
    inversemat <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inversemat <<- inv
  getinverse <- function() inversemat
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Second part: "special" matrix creation
## This section calculates the inverse of the matrix if it has not been previously calculated 
## in which case, it takes the result from the cache

cacheSolve <- function(x, ...) {
  inversemat <- x$getinverse()
  if(!is.null(inversemat))  {
    message("getting cached data")
    return(inversemat)
  }
  matrix1 <- x$get()
  inversemat <- solve(matrix1, ...)
  x$setinverse(inversemat)
  inversemat
}
