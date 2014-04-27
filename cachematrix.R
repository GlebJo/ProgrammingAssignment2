## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix 
##    conctructor   - save initial origin matrix in "origin", set variable "invert" <-  NULL
##    set(m)        - save new origin matrix in parent cache, set variable "invert" in parent cache <<- NULL
##    setinvert(inv)- save new origin matrix inversion (paremeter "inv") in parent cache
##    getorigin()   - returns current value of origin matrix from parent cache
##    getinvert()   - returns current value of "invert" from parent cache
##    

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  origin <- x
  invert <- NULL;
  
  set <- function(m) {
      origin <<- m
      invert<<-NULL
  }
  setinvert <- function(inv) invert<<-inv
  getorigin <- function() origin
  getinvert <- function() invert
  list(set = set, setinvert = setinvert, getorigin = getorigin, getinvert = getinvert)
}


## Write a short comment describing this function
##    check if variable "invert" in x parent cach is not NULL and returns this value from cach
##    else call x$setinvert() (calculate new origin matrix inversion and save it in x parent cache)
##    then returns new origin matrix inversion
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvert()
  if( !is.null(inv)){
    message("getting from cach")
    return(inv)
  }
  org <- x$getorigin()
  inv <- solve(org)
  x$setinvert(inv)
  return(inv)
}
