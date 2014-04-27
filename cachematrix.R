##=========================================================================
## Pair of functions that cache the inverse of a matrix.
##-------------------------------------------------------------------------
## This code has been written with the precious help of Fu Sheng Wang's
## post: "Hope this helps those who are lost" with the objective to explain
## the sample code provided for the second Programming Assignment of the
## Johns Hopkins "R Programming" course by Roger D. Peng, Brian Caffo, PhD, 
## and Jeff Leek on www.coursera.org
##=========================================================================

##-------------------------------------------------------------------------
## This function creates a special "matrix" object that can cache
## its inverse.
##-------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(matrix) m <<- matrix
  getMatrixInverse <- function() m
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
  
}

##-------------------------------------------------------------------------
## This function computes and returns the inverse of a "matrix"  'x'
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not 
## changed), then cacheSolve should retrieve the inverse from the cache.
##-------------------------------------------------------------------------
cacheSolve <- function(x, ...) 
{
  m <- x$getMatrixInverse() #queries the x inverse matrix's cache 
  if(!is.null(m))           #if there is a cache 
  {         
    message("getting the cached inverse matrix")
    return(m)               #just return the cache, no computation needed
  }
  data <- x$get()           #if there's no cache
  m <- solve(data, ...)     #Compute the inverse of a the matrix with the R solve function.
  x$setMatrixInverse(m)     #save the result back to x's cache
  m                         #return the result
}
