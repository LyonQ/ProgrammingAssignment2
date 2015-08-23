## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix creates a special matrix object, and then cacheSolve 
  ## calculates the inverse of the matrix.
  ## If the matrix inverse has already been calculated, it will instead 
  ## find it in the cache and return it, and not calculate it again.
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## The function cacheSolve returns the inverse of a matrix A created with
  ## the makeCacheMatrix function.
  ## If the cached inverse is available, cacheSolve retrieves it, while if
  ## not, it computes, caches, and returns it.
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
