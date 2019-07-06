## Put comments here that give an overall description of what your
## functions do
## These functions are to cache the inverse of a given matrix 

## Write a short comment describing this function
## "makeCacheMatrix" function creates a matrix which cashes its inverse 
##and that matrix is used as the input to the "cachesSolve" function 

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## "cacheSolve" function takes the matrix output from "makeCacheMatrix" as its input  
##and compute the inverse of that matrix
##If the inverse of that matrix is previously calculated "cacheSolve" function
##takes the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)##Computing the inverse of a square matrix 
  x$setinv(m)
  m
}
