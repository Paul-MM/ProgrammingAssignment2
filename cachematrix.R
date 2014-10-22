## The function below enable R to cache potentially time consuming computations. 
## Here a matrix & its inverses are cached using the superassignment operator.

## This function creates a special matrix that will cashe its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                 ###
  set <- function(y) {                      ###
    x <<- y                                 ### variable x assigned with the superassignment operator                  
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m     ## Return a matrix that is the inverse of 'x'
}
