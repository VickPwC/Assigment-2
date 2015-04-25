## VHC Abr 25, 2015
## The following is a pair of functions that cache and compute the  
## inverse of a matrix. 
## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(mtx = matrix()) { 
  
  cache <- NULL 
  
  setMatrix <- function(newvalue) { 
    mtx <<- newvalue
    cache <<- NULL
  }  
  
  setInverse <- function(newvalue) {
    cache <<- newvalue
  }
  
  getCache <- function() { 
    cache
  }
  
  getMatrix <- function() {
    mtx
  }
  
  list(setMatrix = setMatrix, setInverse = setInverse, getMatrix = getMatrix, getCache = getCache) 
} 

## This function computes the inverse of the special 
## "matrix" returned by `makeCacheMatrix` above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## `cacheSolve` should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) { 
  inverse <- x$getCache()
  if(!is.null(inverse)) {
    message("Inverse already exists")
    inverse <- x$getCache() 
  } 
  else {
    message("calculating inverse")
    mtx <- x$getMatrix()
    inverse <- solve(mtx)
    x$setInverse(inverse)
  }
  return(inverse)
} 
