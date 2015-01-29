## Put comments here that give an overall description of what your
## functions do
## The following two functions calculate the inverse of a square matrix. 
## In the first function, makeCacheMatrix, a special "matrix" object is 
## created to cache the inverse of the input matrix. The second 
## function, cacheSolve, calculates the inverse of the "matrix" object 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## and the matrix has not changed, the inverse matrix is retrieved from 
## the cache, thus saving the time and expense of recomputation. 
## These functions use the scoping rules of the R language to preserve 
## states inside an R object.
## For example, a$set(matrix(1:4, 2, 2))
## cacheSolve(a)

## Write a short comment describing this function
## This function creates a list containing a function to 1. set the value
## of the matrix; 2. get the value of the matrix; 3. set the value of the 
## inverse matrix; 4. get the value of the inverse matrix. In this 
## function, the "inv" is used to cache the inverse matrix. When a value 
## is set to the matrix, the cached inverse matrix is cleared.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(p) inv <<- p
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function returns the inverse of the input function. If the 
## inverse matrix exists, it skips the computation and gets the result
## from the cache; otherwise, it calculates the inverse matrix using 
## the "solve" function and sets it in the cache using the "setinv" 
## function. 
## When the value of the matrix is changed with the "set" function, the 
## cached inverse matrix will be cleared. If cacheSolve runs, it will 
## recalculate the inverse matrix and updae its cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv                         ## Return a matrix that is the inverse of 'x'
}
