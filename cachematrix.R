

## This function creates a matrix object that can cache its inverse.
mCacheMatrix <- function(x= matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x} ## function to get matrix x
  setInverse <- function(inverse) {inv <<-inverse}
  getInverse <- function() {inv} ## function to get the inverse of matrix 
  list(set=set, get=get , setInverse=setInverse,getInverse=getInverse)
  
}

##this functions is used to cache data
CacheSolve <- function(x,...) {
  inv <- x$getInverse()
  if(!is.null(inv)){ ## check if the inverse is null
    message("getting Cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...) ## calculate the inverse
  x$setInverse(inv)
  inv ## return the inverse of x 
}
