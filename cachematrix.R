## makeCacheMatrix() allows for creation of a list for gettin and setting Matrix at global level
## cacheSolve() provides functionality to return the Inverse of the Matrix from cache if existing.

## The function makeCacheMatrix() creates a list containg function to perform the following :
##         - set value of the Matrix
##         - get value of the Matrix
##         - set value representing Inverse of the Matrix
##         - get value representing Inverse of the Matrix
##  Super Assignment operator "<<-" is used to set the values at golbal level so that the values can be
##  cached.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## creates a global variable with the input Matrix and thus caches it.
  set <- function(y) {
     x <<- y
     inv <<- NULL
  }
  
  ## returns back the cached input Matrix
  get <- function() x
  
  ## create a global variable containing the Inverse MAtrix and thus caches the data
  setInv <- function(invMtx)inv <<- invMtx
  
  ## returns back the cached Inverse MAtrix
  getInv <- function() inv
  
  ## list containing getters and setters are return by this function
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)  
}


## The function cacheSolve accepts a Matrix as input and returns back the Inverse Matrix
## It should be noted that the Inverse is computed only once for a given Matrix. If the 
## value is found in globally, then that is instantly returned saving the time to computer 
## the Inverse.

cacheSolve <- function(x, ...) {
  
  ## reading the Inverse Matrix data (it might or might not exist)
  inv <- x$getInv()
  
  ## checks to see if a non null Matrix containing Inverse data is returned.
  ## if found then we exit from this function with that value.
  if(!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  
  
  data <- x$get()
  
  ## use solve() to compute the inverse of the matrix
  inv <- solve(data, ...)
  
  message("setting the Inverse data for first time")
  x$setInv(inv)
  
  ## return to the caller, as last line is always the function result
  inv
  
}
