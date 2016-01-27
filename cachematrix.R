## "makeCacheMatrix" : creates a special “matrix” object that can cache its inverse
## makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## "cacheSolve" : computes the inverse of the “matrix” returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, it’ll retrieves the inverse from the cache directly.
## This function assumes that the matrix is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

Test run:
## Creating matrix

> x = rbind(c(7,2,1), c(0,3,-1),c(-3,4,-2))
> m = makeCacheMatrix(x)
> m$get()
     [,1] [,2] [,3]
[1,]    7    2    1
[2,]    0    3   -1
[3,]   -3    4   -2
## No cache in the first run

> cacheSolve(m)
     [,1] [,2] [,3]
[1,]   -2    8   -5
[2,]    3  -11    7
[3,]    9  -34   21

## Retrieving from the cache in the second run
> cacheSolve(m)
getting cached data
     [,1] [,2] [,3]
[1,]   -2    8   -5
[2,]    3  -11    7
[3,]    9  -34   21
> 
