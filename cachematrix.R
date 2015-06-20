## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## rather than computing it repeatedly. Below we provide a pair of functions that cache the inverse of a matrix
## 
## To test that these functions work, you can run the following sequence
# 
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## data=hilbert(3)
## wrapper=makeCacheMatrix(data)
## # Should say that it recalculates
## inverse=cacheSolve(wrapper)
## # Now should say that it gets it from the cache
## inverse=cacheSolve(wrapper)
## # To test that it is really inverse, multiply inverse and original and it should be identity matrix. We test this
## # this way (round is needed to make sure that numbers like 1e-15 are treated as zeros)
## identical(round(inverse%*%wrapper$get()),diag(nrow(wrapper$get())))



## This function creates a wrapper around the original matrix provided. This wrapper gives an access to 4 functions
## get - returns the original matrix
## set - allows you to override the matrix in the wrapper. This also resets cache of inverse matrix
## setInverse - stores the inverse matrix in a cache
## getInverse - returns the inverse matrix from the cach (or null if cache does not contain inverse matrix yet)
## You can use the result returned by this function with cacheSolve function
makeCacheMatrix <- function(original_matrix = matrix()) {
  # This is our cache
  inverse_matrix = NULL
  
  set <- function(matrix_to_set) {
    original_matrix <<- matrix_to_set
    # Make sure we reset our cache once a new matrix is provided
    inverse_matrix <<- NULL
  }
  get <- function() {
    original_matrix
  }  
  setInverse <- function(calculated_inverse){
    inverse_matrix <<- calculated_inverse
  } 
  getInverse <- function() {
    inverse_matrix
  }
  # Return the result which is a list of 4 functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function takes in a wrapper of a matrix created via makeCacheMatrix and returns an inverse of a matrix stored
## in a wrapper (if inverse matrix was already calculated) or recalculates the inverse matrix, stores it in a wrapper
## and returns an inverse matrix (if inverse was not calculated before)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getInverse()
  if(!is.null(inverse_matrix)) {
    print("Getting cached inversed matrix")
    return(inverse_matrix)
  }
  print("Recalculating inverse of a matrix")
  matrix <- x$get()
  inverse_matrix=solve(matrix)
  x$setInverse(inverse_matrix)
  inverse_matrix
}
