## Put comments here that give an overall description of what your function does

## This function creates a matrix object that can cache its inverse
## Usage example: 
##    m<-makeCacheMatrix(matrix(1:4,ncol=2))
##    m$get()
##    m$getinverse()
##    cacheSovle(m)
##  etc...

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(s) m <<- s
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function calculates the inverse of the matrix created with the above function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
