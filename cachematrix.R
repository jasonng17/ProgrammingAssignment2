## Put comments here that give an overall description of what your
## functions do

## Define getters and setters functions and the matrix_store object that persists within the
## environment of makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  matrix_store <- NULL
  get <- function() x     #Get the data
  setmatrix <- function(inverse_matrix) matrix_store <<- inverse_matrix
  getmatrix <- function() matrix_store   #Set the inverse matrix
  list(get = get, 
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } 
  
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setmatrix(inverse_matrix)
  inverse_matrix
}
