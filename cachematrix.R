## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse
## and containing a function to
## get the value of the matrix
## set the value of the matrix
## get the value of the inverse matrix
## set the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ## i is the cached inverse matrix
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
## notice that, x must be a square invertible matrix

cacheSolve <- function(x, ...) {
  ## get the inverse matrix
  i <- x$getinv()
  ## if the matrix is not null, then return the cached matrix
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  ## if the matrix is null, then the inverse matrix is calculated and set 
  ## to the cache
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}
