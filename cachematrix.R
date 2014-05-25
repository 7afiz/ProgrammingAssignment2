## The following two functions are caching the inverse of a matrix because it
## is costly to repeat the computation of the matrix inversion.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL
      
      # set matrix value
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      # get matrix value
      get <- function() x
      
      # set inverse matrix value
      setinverse <- function(inverse) inv <<- inverse
      
      # get inverse matrix value
      getinverse <- function() inv
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
      
      ## checks to see if the inverse has already been calculate.
      ## If TRUE, it gets the inverse from the cache and skips the computation.
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data.")
            return(inv)
      }
      ## Otherwise it calculates the inverse and sets the value of the inverse 
      ## in the cache via the setinverse() function.
      
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv  
}
