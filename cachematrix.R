## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(M = matrix()) {
  Inv <- NULL#Declaration of inv as a new Matrix
  set <- function(y) {# Sets the value of the matrix M in the object
    M <<- y
    Inv <<- NULL
  }
  get <- function() {M}# to get the value of the matrix
  setinverse <- function(inverse) {Inv <<- inverse}#input the inverse once its calculated
  getinverse <- function() {Inv}#to get the stored inverse matrix 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse=getinverse)##definition of the object and their elements
}


## Write a short comment describing this function

cacheSolve <- function(M, ...) {
        ## Return a matrix that is the inverse of 'M'
  Inv <- M$getinverse()#catch if there is a inverse calculated
  if(!is.null(Inv)) {#ask if the inverse exist
    message("getting cached data")#if true then prints or returns the value
    return(Inv)
  }
  data <- M$get()#if not, then gets the matrix
  Inv <- solve(data, ...)#calculates the value
  M$setinverse(Inv)#sets in to the object
  Inv#return the inverted matrix
}
