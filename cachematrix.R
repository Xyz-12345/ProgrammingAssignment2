## Put comments here that give an overall description of what your
## functions do
## Collection of function for creating a matrix and calculate it's inverse, storing the result calculation in cache
## When called again, it retrieves the stored value if it exists.

## Write a short comment describing this function
## This function creates a matrix and methods to access and store it

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function () inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This functions stores the matrix inverse with the matrix given by the makeCacheMatrix function in cache
## and then stores it in cache, when called again if the inverse object exists, it doesn't do the calculation again
## just retrieve from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


x <- makeCacheMatrix(matrix(c(1,2,3, 4), 2, 2))
cacheSolve(x)


