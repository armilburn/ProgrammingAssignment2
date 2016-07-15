# Define two functions that supply a constructor for an inverted matrix object
# and a function that uses the methods defined by that object

# makeCacheMatrix essentially is a constructor for a cache matrix object.
# It merely initialzes the inversion matrix to NULL and then defines
# four methods for the class: 
#     set (sets the matrix to new values and invalidate the inverted matrix)
#     get (returns the matrix itself)
#     setInverse (sets the inverted matrix to something)
#     getInverse (gets the inverted matrix)


makeCacheMatrix <- function(mat = matrix()) {
  #indicate the inverse matrix hasn't been calculated
  inv <- NULL
  
  # set function (method) to reset the contents and invalidate the inverted matrix
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  
  # get function (method) to return contents of the (uninverted) matrix
  get <- function() {
    mat
  }
  
  # setInverse function (method) to set the contents of the inverted matrix
  setInverse <- function(solve) {
    inv <<- solve
  }
  
  # getInverse function (method) to return the contents of the inverted matrix (or NULL!)
  getInverse <- function() {
    inv
  }
  
  # provides list of functions available with this object
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# cacheSolve is essentially a wrapper function for Solve():
# If the matrix has already been inverted, the inverted matrix is returned
# If the matrix has not been inverted, it is inverted and the inverted matrix is stored for later use


cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  if(!is.null(m)) {
    # Yay!  Don't have to invert the matrix
    message("getting cached data")
    return(m)
  }
  
  # Whoops!  Matrix hasn't been inverted.  Get the matrix, invert it, cache the inverted matrix, and return inverted matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

