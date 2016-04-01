## The logic for this set of functions involves the idea that a change in value 
## of an object is recognizable. This idea can be used as a time saving method.
## Imagine a calculation is performed and the 
## value of that calculation then replaces the value of some object. 
## This replaced value can act as a marker to suggest that the calculation 
## was once completed. Rather than repeating the computation, the value 
## of the object, which should be the final value of the calculation can be read
## , rather than reperforming the computation. 
## This is exactly what makeCacheMatrix and cacheSolve do,
## as it will take a matrix and invert it if it has not been 
## inverted. If it has been inverted once before, it will not perform the 
## calculation, instead just providing the value of the earlier performed 
## calculation.

## makeCacheMatrix can be thought of as a list of functions and cacheSolve could
## are the set of logical steps to be taken to ensure that the calculation is 
## not repeated. cacheSolve will check that an object has not been altered from
## its default NULL value. If it has been altered then it would mean that the 
## calculation has presumably been run before and the value of that object is 
## is now the value of that calculation. This should be true if the rest of the 
## instructions of cacheSolve makes it so.

## The makeCacheMatrix function will act to create four functions that will be
## subsequently placed into a list. This list will be manipulated by cacheSolve  
## to determine whether a calculation has already been performed. 

makeCacheMatrix <- function(x = matrix()) {
  neo <- NULL
  set <- function(y) {
    x <<- y
    neo <<- NULL
  }
  get <- function() x
  setinvert <- function(invert) neo <<- invert
  getinvert <- function() neo
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)

}


## The cacheSolve function will use the list provided by makeCacheMatrix. 
## This is possible because makeCacheMatrix is being passed into cacheSolve. 
## The statements of cacheSolve will determine if the NULL value of the 
## object neo have been altered, if not a calculation will be performed 
## and subsequently replace the NULL value of neo. 

cacheSolve <- function(x, ...) {
  neo <- x$getinvert()
  if(!is.null(neo)) {
    message("getting cached data")
    return(neo)
  }
  matrx <- x$get()
  neo <- solve(matrx, ...)
  x$setinvert(neo)
  neo
        ## Return a matrix that is the inverse of 'x'
}
