## The workflow below allows the user to cache the inverse of a regularly used
## matrix to avoid expensive re-computation.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then the cachesolve should retrieve the inverse from 
#the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
       
    if(!is.null(inv)) {
    return(inv)
    }
    
    data <- x$get()
    
    if (det(data) == 0) {
      stop("Connot invert matrix")
    }
    
    inv <- solve(data, ...)
    
    x$setInverse(inv)
    
    inv
}
