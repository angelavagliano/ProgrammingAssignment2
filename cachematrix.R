##  The two functions below create a special object that allow to store 
##  a matrix and cache its inverse (rather than compute it repeatedly).

## The first function, makeCacheMatrix, gives a special matrix that can 
## cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## The function cacheSolve checks if the inverse matrix has been calculated before.
#  2.1 If so, It gets the inverted matrix from the cache and skips the computation.
#  2.2 Otherwise, It calculates the inverted matrix and store it in the cache using the seatinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}