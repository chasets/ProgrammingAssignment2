## Put comments here that give an overall description of what your
## functions do

## TSC 2014-05-18
## create a matrix type object that can get and set 
## its own inverse (using the solve function)
## If the inverse has alread been computed, then getsolve() will return 
## the inverse; if not, use setsolve() to compute the inverse and cache
## it inside the object

## TSC 2014-05-24
## Adding some testing
## Set up the matrices using the magic package
# m3 <- makeCacheMatrix(magic(3))
# m5 <- makeCacheMatrix(magic(5))
# m2001 <- makeCacheMatrix(magic(2001))
## Solve and check for the cache hit
# cacheSolve(m3)
# cacheSolve(m3)
## Timing on a small matrix
# system.time(cacheSolve(m5))
# system.time(cacheSolve(m5))
## Timing on a bigger matrix. 
# system.time(cacheSolve(m2001))
# system.time(cacheSolve(m2001))
## 20 seconds on first; 0 seconds on the second

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  
}


## TSC 2014-05-18
## use makeCacheMatrix to get the (possibly cached) inverse
## of a matrix. 
## If the inverse has alread been computed, then getsolve() will return 
## the inverse; if not, use setsolve() to compute the inverse and cache
## it inside the object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
