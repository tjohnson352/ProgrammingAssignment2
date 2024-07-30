## These functions work together to handle matrix 
## inverses efficiently. The first function, 
## makeCacheMatrix, creates a special matrix that can
## remember its inverse. The second function,
## cacheSolve, calculates the inverse of this special 
## matrix. If the inverse has already been calculated 
## and the matrix hasn't changed, cacheSolve will use 
## the stored inverse to save time.

## makeCacheMatrix: This function creates a special 
## "matrix" that can store its inverse for later use.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function calculates the inverse of 
## the special "matrix" created by makeCacheMatrix. 
## If the inverse has already been calculated and the 
## matrix hasn't changed, cacheSolve will get the inverse 
## from the stored data instead of calculating it again.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
