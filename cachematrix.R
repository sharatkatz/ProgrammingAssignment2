# Usage and test run:
# rawm <- replicate(10, rnorm(10))
# cachedmatrix <- makeCacheMatrix(rawm)
# cacheSolve(cachedmatrix)

# This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(inx = matrix()) {
  # m will store the cached inverse matrix
  m <- NULL
  
  # set the value of theinverse  matrix
  set <- function(y) {
    inx <<- y
    m <<- NULL
  }
  
  # get the value of the inverse matrix
  get <- function() inx
  
  # set the value of the inverse matrix
  setinv <- function(inverse) m <<- inverse
  
  # get the value of the inverse matrix
  getinv <- function() m
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# This function computes the inverse of the special matrix returned by makeCacheMatrix above
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  # If the inverse is already calculated, return it
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # else calculate it
  data <- x$get()
  m <- solve(data, ...)
  
  x$setinv(m)
  
  # return invsere matrix
  m
}



