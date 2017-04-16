## This function creates a special "matrix" object that can cache its inverse.

##  Function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse. 
##  makeCacheMatrix contains 4 functions: set, get, setmean, getmean.
##(a) get is a function that returns the vector z stored in the main function.
##(b) set is a function that changes the vector stored in the main function.
##(c) setmean stores the value of the input in a variable a in the function makeVector.
##(d) getmean returns the value of hte input in a variable a in the function makeVector.

makeCacheMatrix <- function(z = matrix()) {
  a <- NULL
  set <- function(y) {
    z <<- y
    a <<- NULL
  }
  get <- function() z
  setinverse <- function(solve) a <<- solve
  getinverse <- function() a
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve uses the return of makeCacheMatrix to calculate the inverse of the
## matrix. 


cacheSolve <- function(z, ...) {
  a <- z$getinverse()
  ## Return a matrix that is the inverse of 'z'
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- z$get()
  a <- solve(data, ...)
  z$setinverse(a)
  a
}