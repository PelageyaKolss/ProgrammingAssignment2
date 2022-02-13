

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
    get <- function() x
     setInverse <- function() inv <<- solve(x) #calculate the inverse
     getInverse <- function() inv
     list(set = set,
          get = get,
          setInverse = setInverse,
         getInverse = getInverse)
 }
}


## This function creates a special "matrix" object that can cache its inverse.

cacheSolve <- function(x, ...) {
         inv <- x$getinv()
     if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
   x$setinv(inv)
    inv
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
