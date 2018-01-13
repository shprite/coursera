## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this function initialises the matrix and the variable to store the inverse of the matrix - inv

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# this function calculates the inverse of the matrix using the inbuilt solve function in r. first it looks to see whether the inverse has been calculated and stored already. if it hasn't been calculated, it then calculates the inverse of the matrix and then stores is it in the cache where it can be retrieved from

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inv <- x$getinv()
  if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
  }
  data<-x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
