
## The objective of these 2 functions is to cache the inverse of a matrix
## The assumption taken here is that the matrix passed as arguments will be invertable

## The function makeCacheMatrix can be used to create a special matrix object which can cache its inverse
## It consists of a list of 4 functions which help in setting and getting the matrix and setting and getting the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  CacheMatrix <- NULL
  
  setMatrix <- function(y) {
    
    x <<- y
    CacheMatrix <<- NULL
    
  }
  
  getMatrix <- function() x
  
  setInverseMatrix <- function(InvMat) CacheMatrix <<- InvMat
  
  getInverseMatrix <- function() CacheMatrix
  
  list(set = setMatrix, get = getMatrix, setInv = setInverseMatrix, getInv = getInverseMatrix)
  
}

## The function cacheSolve can be used to compute the inverse of the special matrix object returned by makeCacheMatrix
## It finds out whether the inverse has already been computed and returns in the same, else it computes and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  
  CacheMatrix <- x$getInv()
  
  if(!is.null(CacheMatrix)) {
    message("getting cached matrix")
    return(CacheMatrix)
  }
  
  InvertMat <- x$getMatrix()
  
  CacheMatrix <- solve(InvertMat)
  x$setInv(CacheMatrix)
  
  CacheMatrix
  
}