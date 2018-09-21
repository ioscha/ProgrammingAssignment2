## Created on 21.09.2018
##
## Below are two functions that are used to create a special object that stores
## a numeric matrix and caches its inverse.

## This function stores a numeric matrix with a cache for its inverse.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}
x <- makeCacheMatrix(testmatrix)

## This function calculates the inverse of the matrix with cache created in the function above. If there is already an inverse
## matrix in the cache, it directly returns the inverse without calculating it again.      
cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
           message("getting cached data")
           return(inv)
     }
     data <-  x$get()
     inv <- solve(data,...)
     x$setinverse(inv)
     inv
}
