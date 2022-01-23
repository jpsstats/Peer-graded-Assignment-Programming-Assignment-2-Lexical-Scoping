## Put comments here that give an overall description of what your
## functions do
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
##makeCacheMatrix consists set, get, setinverse, getinverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x            #function to get matrix x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
    
      #function to obtain inverse 
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
##this is used to cache data
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {     #checking whether value is null
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...) #calculates inverse value 
  x$setInverse(inv)#returns inverse value 
  inv
}
