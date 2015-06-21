# The makeCacheMatrix function, makeVector creates a special "vector",
# which is really a list containing a function to:
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

# This function calculates the inverse matrix and store it at cache in order to save processing capacity
# during further calculations

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- inv
  getinv <- function() inv
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


# The following function calculates matrix inversion of the special "vector" 
# created with makeCacheMatrix function.
# It first checks to see if the mean has already been calculated.
# If so, it gets the mean from the cache and skips the computation.
# Otherwise, it calculates the mean of the data and sets the value of the mean in the cache
# via the setinv function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("There are data stored in cache. Recovering Data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
