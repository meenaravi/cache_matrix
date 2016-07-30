## Computing a inverse matrix is a tough and costly task to perform each and everytime. 
## To avoid such multiple computations an inverse matrix is chached.

##Creates a list with a function which sets and gets the values of both matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) invmatrix <<- solve()
  getInverse <- function() invmatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This first checks for the existance of inverse function.
## If it exists computation is skipped or else inverse matrix is calculated and then caches 
## it via setInverse Function.
cacheSolve <- function(x, ...) {
  invmatrix <- x$getInverse()
  if(!is.null(invmatrix)) {
    message("gettingg cached data")
    return(invmatrix)
  }
  data <- x$get()
  invmatrix <- solve(data, ...)
  x$setInverse(invmatrix)        ## Return a matrix that is the inverse of 'x'
}
