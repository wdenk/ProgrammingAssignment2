## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a "CacheMatrix" object.
## The parameter m stores the inverse of the matrix x. m is initalized as NULL.
## setinv and getinv store and retrive the inverse in m.
## m is NULL until setinv is called.

makeCacheMatrix <- function(x = matrix()) {

  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function calles getinv() on the matrix x.
## If the inverse wasn't calculated yet m is NULL and m <- solve(data,...)
## calucaltes the inverse and x$setinv(m) stores the value in the "CacheMatrix" Object.
## The second time x$getinv() is called m won't be NULL, it stores the cached inverse.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}
