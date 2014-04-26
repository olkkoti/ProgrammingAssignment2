## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list containing functions:
## 1. set(matrix) sets the matrix
## 2. get() returns the matrix
## 3. setinverse(inverse) sets the inverse of the set matrix
## 4. getinverse() returns the inverse
## Also, the matrix can be set by giving it as an argument.
## The set matrix must be invertible.
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(matrix) {
    x <<- matrix
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(newInverse) inverse <<- newInverse
  
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of a given special matrix.
## First argument must be a special matrix created by makeCacheMatrix().
## If the special matrix has already the inverse cached,
## it is not computed again.
cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()
  
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinverse(inverse)
  inverse
}
