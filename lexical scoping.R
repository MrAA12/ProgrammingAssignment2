makeCacheMatrix <- function(y= matrix()) {
  # y is a square invertible matrix.
  m<- NULL
  set <- function(x) {
    y <<- x
    m <<- NULL
  }
  get <- function() y         # gets the value of the matrix
  setinverse <- function(inverse) m <<- inverse      # sets the value of the invertible matrix
  getinverse <- function() m         # gets the value of the invertible matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
# the list is used as the input to cacheSolve() function.

cacheSolve <- function(y, ...) {
  m <- y$getinverse()
  # The Code below applies if inverse has already been calculated.
  if (!is.null(m)) {
    #getting from the cache and skipping the computation.
    message("getting cached data")
    return(m)      # returns the invertible matrix.
  }
  # if the above is not true, then calculates the inverse via the setinverse function.
  data <- y$get()                  # gets the original matrix function.
  m <- solve(data, ...)            # uses solve function to inverse the matrix.
  y$setinverse(m)
  m         # returns a matrix that is inverse of y.
}
