## Put comments here that give an overall description of what your
## functions do

## Function to create an object that can cache a matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setmatrix <- function(solve) m <<- solve
     getmatrix <- function() m
     list(set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}

## function that computes de inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x = matrix(), ...) {
     m <- x$getmatrix()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     matrix <- x$get()
     m <- solve(matrix, ...)
     x$setmatrix(m)
     m
}