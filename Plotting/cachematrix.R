## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## I set the input x as a matrix
## And, I set the solved value "s" as a null
## then I changed every reference to "mean" to "solve"

makeCacheMatrix <- function(x = matrix()) {
     inv = NULL
     set = function(y){
          x <<- y
          inv <<- NULL
          
     }
     get = function()x
     setinv = function(inverse) inv <<- inverse
     getinv = function() inv
     list(set = set, get = get, setinv = setinv, getinv = getinv)
     
}

## Write a short comment describing this function
## I changed "mean" to "solve" and "m" to "s"
## If the inverse has already been calculated, 
## then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     s <- x$getsolve()
     if(!is.null(s)) {
          message("getting inversed matrix")
          return(s)
     }
     data <- x$get()
     s <- solve(data, ...)
     x$setsolve(s)
     
     }
