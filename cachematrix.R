## The functions allow the storage of a matrix's inverse in the cache memory 
## for repetitive use


## Function makeCacheMatrix creates a matrix object with methods to get and set 
## the matrix itself and its inverse 
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInv <- function(inverseM) invMatrix <<- inverseM
  getInv <- function() invMatrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## function cacheSolve checks if a matrix's inverse is in the cache memory. If not, it 
## calculates the inverse and saves it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInv()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setInv(invMatrix)
  invMatrix
}
