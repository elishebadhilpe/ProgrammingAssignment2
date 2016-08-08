## These functions deal with caching the inverses of matrices

## This function creates a special "matrix" object that can cache its inverse in four simple steps.

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function computes the inverse of the matrix produced by the above function. 
#  If the matrix is unchanged and an inverse was already calculated, `cacheSolve` can retrieve the inverse from the pre-existing cache.

cacheSolve <- function(x, ...) {
    i<- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i<- solve(data, ...)
    x$setinv(i)
    i

        ## Return a matrix that is the inverse of 'x'
}
