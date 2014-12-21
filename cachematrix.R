## The second programming assignment
## write an R function to cache potentially time-consuming computations

## makeCacheMatrix is to create an object that will be used to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setMatrix <- function(inv_matrix) inv <<- inv_matrix
  getMatrix <- function() inv
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## cacheSolve is the function to cache the inverse of the matrix if already existed, or to create the inverse if not existed

cacheSolve <- function(x, ...) {
  inv <- x$getMatrix()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setMatrix(inv)
  inv
}
