## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## ############
##  makeCacheMatrix 
## #################
##
##  Version
##   1.0 - 2015.11.21 - João Costa - Creation
##
## #################
## 
##  This function creates a special "matrix" object 
##  that can cache its inverse.
##  It's really a list containing functions to
##
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse matrix
##  get the value of the inverse matrix
##
## #################
## 
##  input: the matrix supplied must be always invertible
##
##  output: list(set, get, setinverse, getinverse)
##
## #################
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
##
## ############
##  cacheSolve 
## ############
##
##  Version
##   1.0 - 2015.11.21 - João Costa - Creation
##
## ############
##
##  This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already 
##  been calculated (and the matrix has not changed), then the 
##  cachesolve should retrieve the inverse from the cache.
##
## ############
##
##  input: list(set, get, setinverse, getinverse)
##
##  output: the inverse matrix
##
## ############
cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
  
        ## Return a matrix that is the inverse of 'x'
}
