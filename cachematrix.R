## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
# The function makeCacheMatrix creates a list to: a) set the matrix value, b) get matrix value, c) set matrix inverse d) get matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set <- function(y) {
    x<<-y
    Inverse <<- NULL
  }
  
  get <- function () x
  
  setInverse <- function(m_inv) Inverse <<- m_inv
  getInverse <- function() Inverse
  
  list(set=set, get=get, setInverse = setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed),
#then cacheSolve should retrieve the inverse from the cache.

#Assumption: The inverse of the matrix exists.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inverse <- x$getInverse()
  if(!is.null(Inverse)) {     #Only if matrix is invertible
    message("Retrieving the cache data")
    return (Inverse)
  }
  data <-x$get()
  Inverse<-solve(data)
  x$setInverse(Inverse)
  Inverse
}


