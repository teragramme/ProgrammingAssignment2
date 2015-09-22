## makeCacheMatrix and cacheSolve are two functions that work together to compute and cache 
## the inverse of a user-input matrix.  The input matrix should be a SQUARE INVERTIBLE matrix.


## makeCacheMatrix takes an input matrix 'x' (and its inverse matrix computed by 'cacheSolve') and caches them.
## It returns four functions that allow the input and inverted matrices to be retrieved, or new ones set.

makeCacheMatrix <- function(x = matrix()) {

      cacheMatrix <- NULL
      
      set <- function(y) {
            x <<- y   # sets input y as global x so it can be used in cacheSolve
            cacheMatrix <<- NULL   # empties the global cache
      }
      get <- function() x
      setInv <- function(invMatrix) cacheMatrix <<- invMatrix
      getInv <- function() cacheMatrix 

      list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve uses the 'solve' function and functions returned by 'makeCacheMatrix' and returns
## the inverse of the user-input matrix 'x' either by computing it or by retrieving it from cache.

cacheSolve <- function(x, ...) {

      cacheMatrix <- x$getInv()  # is there an inverse matrix cached?
      
      ## if cached, display message and return inverse
      if(!is.null(cacheMatrix)) {  
            message("getting cached inverse matrix")
            return(cacheMatrix)
      }
      
      ## if not cached, create inverse from input matrix
      newMatrix <- x$get() 
      cacheMatrix <- solve(newMatrix, ...)
      x$setInv(cacheMatrix)
      cacheMatrix
}




##################################################
## A 2x2 TEST CASE:
##
## > source("cachematrix.R")                        # load source file
##
## > my_matrix <- matrix(c(6, 36, 256, 1512), 2,2)  # input is a square invertible matrix
## >
## > cache_matrix <- makeCacheMatrix(my_matrix)     # pass it into 'makeCacheMatrix' function
## >
## > cache_matrix$get()                             # take a peak at the input matrix
##      [,1] [,2]
## [1,]    6  256
## [2,]   36 1512
## >
## > cacheSolve(cache_matrix)                       # pass it into 'cacheSolve' to compute inverse
##        [,1]        [,2]
## [1,] -10.50  1.77777778
## [2,]   0.25 -0.04166667
## >
## > inverse <- cacheSolve(cache_matrix)            # FINAL CHECK: reload inverse matrix, into a variable 'inverse'...
## getting cached inverse matrix                    # it displays a confirmatory message...
## >
## > inverse %*% my_matrix                          # multiply INVERSE and INPUT matrices...
##      [,1] [,2]                                   # alt use: cacheSolve(cache_matrix) %*% cache_matrix$get()
## [1,]    1    0                                   # returns the IDENTITY matrix - functions are OK!
## [2,]    0    1
## >

