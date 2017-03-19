## The makeCacheMatrix creates the special matrix object that cahce its inverse
## Further this makeCacheMatrix sets and gets the value of the matrix object
## 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setMatrixInv <- function(inverse) matinv <<- inverse
  getMatrixInv <- function() matinv
  list(set = set, get = get,
       setMatrixInv = setMatrixInv,
       getMatrixInv = getMatrixInv)
}




## This function computers the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has alrady been calculated and the matrix has not changed, then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        matinv <- x$getMatrixInv()
  if(!is.null(matinv)) {
      message("getting cached data")
     return(matinv)
  }
  mat <- x$get()
  print(mat)
  matinv <- solve(mat, ...)
  x$setMatrixInv(matinv)
  matinv
}
