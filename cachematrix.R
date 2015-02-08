## Put comments here that give an overall description of what your
## functions do

## this function is a list of functions that store and retrieve
## a matrix and its inverse in the parent environment

makeCacheMatrix <- function(cur_matrix = matrix()) {
  # clear out the local inverse so the parent environment is searched for it
  inverse <- NULL
  
  # cache a new matrix and clear out the old inverse from the parent (ie the cache)
  set <- function(new_matrix) {
    cur_matrix <<- new_matrix
    inverse <<- NULL
  }
  
  # retrieve the current matrix from the cache (parent) environment
  get <- function() cur_matrix
  
  # create and cache the inverse of the new matrix
  setinverse <- function(cur_matrix) inverse <<- solve(cur_matrix)
  
  # retrieve the current inverse from the global environment
  getinverse <- function() inverse
  
  # create the list for calling the local methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
