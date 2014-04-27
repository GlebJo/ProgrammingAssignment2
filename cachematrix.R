## Put comments here that give an overall description of what your
## functions do
#
#   makeCacheMatrix 
#     returns an "vector" represents
#       properies:
#         origin          - origin matrix
#         invert          - NULL or inversion of origin 
#       methods:
#         set(m)          - set new origin matrix in "origin"
#         setinvert(inv)  - save new inversion of origin matrix
#         getorigin()     - returns current origin matrix
#         getinvert()     - returns current value of invert
#
#   Usage:
#     makeCacheMatrix(x)  - create new "vector" from origin matrix x
#     makeCacheMatrix()   - create new "vector" with empty origin matrix
#
#   cacheSolve
#     if invertion of current origin matrix is not caculate yet, 
#     then calculates new inversion of current origin matrix
#     else returns calculated inverted matrix from cache
#
#   Usage:
#     cacheSolve(x) - caclate inversion of x$origin or return x$inversion, if it already calculated

## Write a short comment describing this function
## makeCacheMatrix 
##
##    conctructor   - save initial origin matrix in "origin", set variable "invert" <-  NULL
##    set(m)        - save new origin matrix in parent cache, set variable "invert" in parent cache <<- NULL
##    setinvert(inv)- save new origin matrix inversion (paremeter "inv") in parent cache
##    getorigin()   - returns current value of origin matrix from parent cache
##    getinvert()   - returns current value of "invert" from parent cache
##    

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
##cacheSolve
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
