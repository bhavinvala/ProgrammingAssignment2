#makeCacheMatrix function creates inverse matrix of the input Matrix and store it for future use to avoid recalculation of inverse Matrix.
#Assume that the matrix supplied is always invertible.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv,getinv = getinv) 
}

##cacheSolve function retrive the Inverse Matrix if it's already available.
##if it's not, then calculate the inverse matrix and store it for future use.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m        
}