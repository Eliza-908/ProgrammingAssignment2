## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than computing 
## it repeatedly. These functions cache the inverse of a matrix, given that
## the matrix is invertible 

## This function achieves four tasks with funtions
## 1. Sets the value of the matrix 
## 2. Gets the value of the matrix 
## 3. Sets the value of the inverse of the matrix
## 4. Gets the value of the inverse of the matrix
## makeCacheMatrix returns a list of all of these tasks 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache via 
## the setinverse function. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("getting the cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
