## makeCacheMatrix - this function creates a special "matrix" object that can cache its inverse

## initialize objects x and inv
## define the functions for objects of type makeCacheMatrix - set, get, setInverse and getInverse
## set changes the vector stored in the main function
## get returns the vector x stored in the main function
## setmean and getmean store the value of the input in a variable inv
## return a list() to create a new object 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## cacheSolve - this function computes the inverse of the special "matrix" returned by makeCacheMatrix above 
## if the inverse has already been calculated (and the matrix has not changed) 
## then the cachesolve should retrieve the inverse from the cache

## list elements in cacheSolve() are defined with names
## this allows us to access these functions with the $ form of the extract operator

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
