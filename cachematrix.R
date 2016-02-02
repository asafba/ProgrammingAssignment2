## These pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object that is really a list of 4 functions:
## 1.  set the value of matrix, 2. get that matrix, 3. set the computed inverse matrix, 4. get that inverse matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function compute the inverse of the special "matrix" created with the above function. 
## It checks to see if the inverse matrix has already been computed: If so, it gets the inverse matrix from the cache. 
## Otherwise, it computes the inverse of the matrix and sets the new matrix in the cache via the setinverse function. 

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
