## The two functions are used to first create a special kind of matrix-like object
## that will be used to store a matrix and cache it's inverse value.

## This function creates a matrix which actually gives a list of "instructions":
## set and get the value of the matrix, and set and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function first checks if the inverse has already been calculated. If so, then 
## it skips the calculation. Otherwise, it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
