## The functions demonstrate caching a variable using R environments
## See further info below

## Creates a special matrix object that allows caching the inverse once its calculated. x param is a matrix.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Given a special matrix created with makeCacheMatrix, calculates the inverse if it has not already been calculated
## Assumes invesrse of matrix can be calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinv()
        if (!is.null(m))
        {
          message("getting cached data")
          return(m)
        }
        data<-x$get()
        m<-solve(data)
        x$setinv(m)
        m
}
