## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function takes a matrix as an argument and creates a list object with four functions
## and a variable inv which stores the inverse of the matrix. The function set can be 
## used to change the data in the matrix. get returns the matrix. The functions setinv
## and getinv can be used to set the value of inv variable and return the cached value of inv
## respectively.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

## The function takes the special matrix created from the function makeCacheMatrix and 
## returns a matrix that is the inverse. If the value of inverse is cached, it is fetched 
## and returned, else the data is extracted, inverse computed and cached. The computed 
## inverse is then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached value")
    return (inv)
  }
  matrix <- x$get()
  inv <- solve(matrix,...)
  x$setinv(inv)
  inv
}
