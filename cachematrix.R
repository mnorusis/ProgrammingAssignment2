## Coursera: R Programming
##    Week 3: Programming Assignment
## Calcuate/cache the inversion of a matrix

## create a 'special' matrix that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Returns the inverse of a matrix
##    Compute, cache and return the inverse of the matrix, if it has not been computed already
##    Return the inverse of the matrix, if previousl computed
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  if(!is.null(i)){
    message("get cached data")
    return(i)
  }
  data <- x$get()
  print(data)
  i <- solve(data, ...)
  x$setInverse(i)
  i
  
}