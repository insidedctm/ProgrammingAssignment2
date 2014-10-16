## A pair of functions that provide a way of caching a matrix inverse
## 

## makeCacheMatrix caches the provided matrix
## parameters: x - a matrix
## returns: a "cached matrix" implemented as a list 
##          providing functions that are required by the 
##          cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(mtx) m <<- mtx
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve can be called with the "cached marix" returned
## by makeCacheMatrix to return the inverse of the matrix.  
## The first time cacheSolve is called for a cached matrix
## a matrix 'solve' is called to generate the inverse. Subsequent
## calls will return a cached inverse rather than calling solve
## again.
## parameters: x - the cached matrix
##             ... - any other parameters to be passed to solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mtx <- x$getmatrix()
        if (!is.null(mtx)) {
          message("getting cached matrix")
          return(mtx)
        }
        data <- x$get()
        mtx <-solve(data, ...)
        x$setmatrix(mtx)
        mtx
}
