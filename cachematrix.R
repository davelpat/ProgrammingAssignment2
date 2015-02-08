## Put comments here that give an overall description of what your
## functions do

## this function is a list of functions that store and retrieve
## a matrix and its inverse in the parent environment

makeCacheMatrix <- function(a_matrix = matrix()) {
  # clear out the local inverse so the parent environment is searched for it
  inverse <- NULL
  
  # cache a new matrix and clear out the old inverse from the parent (ie the cache)
  set <- function(a_matrix) {
    cur_matrix <<- a_matrix
    inverse <<- NULL
  }
  
  # retrieve the current matrix from the cache (parent) environment
  get <- function() cur_matrix
  
  # cache the inverse of the new matrix
  setinverse <- function(a_matrix) inverse <<- a_matrix
  
  # retrieve the current inverse from the global environment
  getinverse <- function() inverse
  
  # create the list for calling the local methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(a_matrix, ...) {
  # get the cached inverse, assuming it exists
  inverse <- a_matrix$getinverse()
  # test the assumption
  if(is.null(inverse)) {
    # no cached inverse, so calculate it and cache it
    data <- a_matrix$get()
    inverse <- solve(data, ...)
    a_matrix$setinverse(inverse)
  } else {
    message("getting cached data")
    #return(inverse) 
  }
  ## Return a matrix that is the inverse of 'a_matrix'
  inverse
}
