## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### The makeCacheMatrix function create a special matrix and store its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

### cacheSolve function compute the inverse matrix which is returned by makeCacheMatrix function.
### If the inverse matrix was already computed and the matrix still the same, then the function will return
### the message "getting cached data" and the same result. If the matrix change, then will compute a new inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}


### In order to test both functions I created a matrix called A, then enter A into the makeCacheMatrix function 
### and I saved into a matrix_t object. Finally I evaluated the cacheSolve function with matrix_t object to compute 
### the inverse matrix. When I ran the cacheSolve for the second time, the function return the message "getting cached data"
### and the same result.
A <- matrix(c(1, -1, 2, -2, 0, 4, 0, -2, 7), nrow=3, ncol=3)
matrix_t <- makeCacheMatrix(A)
cacheSolve(matrix_t)

