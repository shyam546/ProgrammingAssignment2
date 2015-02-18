## First function, makeCacheMatrix() when called for the first time, takes a 
  # matrix as its input and returns a list of functions - 
  # get, set, getinverse and setinverse. 

## Second function cacheSolve() checks if the inverse of the matrix is 
  # already available in the cache by calling on the getinverse()
  # function. If unavailable, it proceeds to obtain the matrix
  # calculate its inverse, and store the inverse in the cache. 
 
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)    
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #obtain inverse stored in cache.
  inv <- x$getinverse()
  
  #if inverse is available, not NULL, all is good. Return value of inverse.
  
  if(!is.null(inv)) {
    message("getting cached data")
    return (inv)    
  }
  
  # if inverse is NULL in cache, proceed to get the matrix stored in cache,
  data <- x$get()
  
  # calculate inverse
  inv <- solve(data)
  
  # update cache with inverse
  x$setinverse(inv)
  
  #return inverse
  inv
}
