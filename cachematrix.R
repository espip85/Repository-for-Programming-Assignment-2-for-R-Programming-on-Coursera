## This pair of functions will cache the 
## inverse of a matrix



##  makeCacheMatrix: This function creates a matrix object that 
## 					 can cache its inverse.
##
## It is really a list containing a function to:
## 	set the vaule of the matrix
## 	get the value of the matrix
## 	set the value of the inverse
## 	get the value of the inverse


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


## cacheSolve: This function computes the inverse of the matrix
## 		returned by makeCacheMatrix. It first check if the inverse 
##	    has already been calculated. If it was already calculated 
##		it gets the inverse from the cache and skip the computation. 
##		If it has not been calculated it calculates the inverse of the 
##		matrix and sets the value of the inverse in the cache via 
##		the setinverse function.


cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
  if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv

}

## To test the functions:

A <- matrix(c(4,2,7,6),2,2)
A.cache <- makeCacheMatrix(A)
A.solve <- cacheSolve(A.cache)
A.solve