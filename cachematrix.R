## R programming assignment 2: Caching the inverse of a matrix
## The following functions perform the following:  
## 1> The first function creates a list of functions 
## in order set the matrix value, call it, set its 
## inverse into cache, and call to get the inverse.
## 2> The 2nd function inputs a matrix and checks if   
## its inverse has already been calculated. If yes it
## prints the inverse from cache, and if no it calculates
## the inverse and prints it

## This function creates 4 functions which set a certain 
## value to a cache and later get it from cache. the first two
## are set and get which pertain to the input matrix. The next 
## two are for the inverse of the matrix as done in the cacheSolve()
## function next.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function inputs a matrix, checks if the inverse 
## is cached or not, using the getinv() function defined 
## previously. If it is present it returns the inverse 
## and if not, then the inverse is calculated using solve() 
## and the result returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  A <- x$get()
  inv <- solve(A, ...)
  x$setinv(inv)
  inv
}

