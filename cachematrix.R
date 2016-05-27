
## makeCacheMatrix caches the matrix and its inverse, which can be called later on without the need to recompute
## given a matrix, the function returns a list with the matrix and its inverse, if invertible

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve uses an object as input with the matrix and its inverse

cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'matrixje', which is the matrix associated to m
  m
  inv <- m$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrixje <- m$get()
  matrixje
  inv <- solve(matrixje)
  m$setinverse(inv)
  inv
  
  
}
