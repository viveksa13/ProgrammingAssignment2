# The first function, makeCacheMatrix calculates the inverse of a matrix :
# This function creates a matrix object that has methods to 
# set and get matrix and its inverse.
# It contains a list containing a function to
  # 1. set the value of the matrix
  # 2. get the value of the matrix
  # 3. setinverse the value of the inverse
  # 4. getinverse the value of the inverse
#
# function cacheSolve: 
# This function does the inverse of the matrix returned by makeCacheMatrix function.
# If the inverse has already been calculated then the
# cachesolve returns the cached value of matrix x as m from the getter function

# function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #: m > holds the cache of the matrix inverse
  m <- NULL
  set <- function(y) {
    x <<- y     #: set the input matrix x in the parent env
    m <<- NULL  #: intiliaze the inverse as null for the new matrix set
  }
  get <- function() x  #: Return the set matrix x
  
  #: set the matrix inverse to cached m
  setinverse <- function(mInverse) m <<- mInverse
  #: get the cached matrix inverse of x as m
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# function cacheSolve  does the inverse of the matrix 
# returned by makeCacheMatrix function and sets the cache of the inverse. 
cacheSolve <- function(x, ...) {
  mInv <- x$getinverse()
  if(!is.null(mInv)) {
    #: inverse cache exists
    message("getting cached data")
    return(mInv)  #: return the cached inverse of x
  }
  #: get the matrix as data
  data <- x$get()
  mInv <- solve(data, ...)  #: calcuate inverse using solve function
  x$setinverse(mInv)  #: cache the calcuated inverse of matrix
  mInv  #: return the inverse of matrix x
}
