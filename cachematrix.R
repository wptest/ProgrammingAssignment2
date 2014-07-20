## makeCacheMatrix and cacheSolve created an inverse of matrix and stores in cache
## If the matrix has already been solved, it is returned instead of recalculated
## NOTE: this functions assume the martix has an inverse

## makeCacheMatrix received a matrix and stores it in cache

makeCacheMatrix <- function(x = matrix()) {

  ## creates the 'object' for to be stored
  m <- NULL

  ## sets the function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## gets the function
  get <- function() x

  ## sets in the inverse in cache
  setinverse <- function(solveMatrix) m <<- solveMatrix

  ## retreives the inverse in cache
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve receives a matrix, 
## if found, it returns the inverse from cache
## if not found, solves the function, saves it to cache, and returns the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  ## check for matrix in cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## call makeCacheMatrix
  data <- x$get()
  ## solve inverse of matrix
  m <- solve(data, ...)
  ## call makeCacheMatrix to store inverse in cache
  x$setinverse(m)
  ## return cache
  m
  
}
