## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix() is a function that caches/stores and retrieves
## cached/stored matrices
## cacheSolve() is a function that calculates the inverse of the
## cached matrix if it does not already exist


## Write a short comment describing this function
## set() functions stores matrices, while get() funtions retrieves stored matrices

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## First checks if any inversed matrix is already cached and return that matrix
## If no inversed matrix was already cached, retrieves original matrix and
## solves for its inverse and caches it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
