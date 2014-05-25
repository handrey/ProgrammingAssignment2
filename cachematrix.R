## The function will prepare an list of functions that will avoid a recalculation 
## of the inverse of a matrix.

## This function makes a list of function that contains four functions to
## execute some operations to the next function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {
              x <<- y
              m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

}


## This function efectivellty calculate the inverse of a matrix if it's not
## already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
      if(!is.null(m)) {
              message("getting cached data")
              return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m


}
