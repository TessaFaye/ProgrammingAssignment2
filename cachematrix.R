## Put comments here that give an overall description of what your
## functions do

## This first function maeks a special "matrix", which is really a
## list containing a function to:
## 1. set teh value of the matrix
## 2. get th value of the matrix
## 3. set the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(x) inv <<- solve(x)
     getinv <- function() inv
     list(set=set, get=get,
          setinv=setinv,
          getinv=getinv)
}


## This function compute the inverse of the special type of
## matrix that is the output of the makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached inverse")
          return(inv)
     }
     mat <- x$get()
     inv <- solve(mat)
     x$setinv(inv)
     inv     
}
