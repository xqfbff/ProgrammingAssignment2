## The second programming assignment
## write an R function to cache potentially time-consuming computations

## makeCacheMatrix is to create an object that will be used to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {   #input x should be a matrix
  inv <- NULL                                 #set inv to NULL at the beginning
  set <- function(y) {                        #set function is used to set another value (y) to x later if desired
    x <<- y
    inv <<- NULL
  }
  get <- function() x                         #get function is to return the matrix x input in the beginning
  setMatrix <- function(inv_matrix) inv <<- inv_matrix        #setMatrix function is to cache the inverse matrix
  getMatrix <- function() inv                 #getMatrix function is to return to cached inverse matrix
  list(set = set, get = get,                  #makeCacheMatrix returns a list of function that can be viewed as an object
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## cacheSolve is the function to cache the inverse of the matrix if already existed, or to create the inverse if not existed

cacheSolve <- function(x, ...) {              #input is the object created by makeCacheMatrix
  inv <- x$getMatrix()                        #get the inverse matrix of the object
  if(!is.null(inv)) {                         #if there exists the inverse matrix (i.e. inv is not null), return the cached value
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()                             #else get the original matrix from the object
  inv <- solve(data, ...)                     #solve for the inverse matrix
  x$setMatrix(inv)                            #set the inverse matrix to inv
  inv                                         #return inv
}
