## Set of 2 functions used to cache the result of a (costly) inversion of matrix
# usage example:
#   x <- matrix(...)
#   cache.x <- makeCacheMatrix(x)
#   xi.1 <- cacheSolve(cache.x) # 1st call: calculate and return the inversion of x
#   xi.2 <- cacheSolve(cache.x) # will used the precedent call cached result

## This function returns a list of 4 functions, used to create a cache for a given matrix 
# (using a different environement). It allows stored and access to the cached inverse matrix
# the List returned contains the following functions 
# $get: return the matrix x
# $set: store in cache the matrix x (and reset to NULL the cached inverse)
# $get.inv: return the cached inverse of x, or NULL if not previously cached
# $set.inv: store in cache the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  set.inv <- function(inv.x) ix <<- inv.x
  get.inv <- function() ix
  list(
    set = set, 
    get = get,
    set.inv = set.inv,
    get.inv = get.inv
    )
}


## Return a matrix that is the inverse of cached value of x
# (using the special list created by the above function)
# It will use the cached value, if it exists, 
# otherwise will calculate and return the inverse, but also store it in the cache
cacheSolve <- function(x, ...) {
  i <- x$get.inv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
    }
  data <- x$get()
  i <- solve(data, ...)
  x$set.inv(i)
  i    
}
