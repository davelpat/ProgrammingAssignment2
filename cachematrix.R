## this pair of functions creates a singleton inverse for a given matrix
## and uses the parent environment to cache the matrix and inverse pair
##
## makeCacheMatrix is used to create the initial data structure and to
## cache and retrieve the matrix and its inverse. 
##
## cacheSolve generates the inverse of the given matrix
## or returns the cached inverse if it exists
##
## A typical sequence would be:
##
## cm <- makeCacheMatrix()  # create the data structure
## cm$set(some_matrix)      # cache the matrix
## cacheSolve(cm)           # generate, cache, and return the inverse
## cacheSolve(cm)           # return the cached inverse

## this function creates a CacheMatrix and the functions that store and
## retrieve the CacheMatrix and its inverse in the parent environment

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

## this function returns the cached inverse of the CacheMatrix if it exists
## or calculates and caches the inverse if it does not exist
## in either case, it returns the inverse of the CacheMatrix passed to it

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
  }
  ## Return a matrix that is the inverse of 'a_matrix'
  inverse
}
