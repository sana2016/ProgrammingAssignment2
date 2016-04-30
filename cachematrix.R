## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly
## This function compute the inverse of a matrix and cache it in order 
## to return the cached inverted matrix it if inversion 
## is called with the same matrix

## The following function takes an argument of class matrix, 
## it returns a list of 4 functions : 
## the first one to set the matrix, 
## the second one to get the matrix, 
## the third one to set the inverse of the matrix
## the fourth one to get the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special "vector" 
## created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
