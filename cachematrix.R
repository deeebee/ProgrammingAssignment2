## This code allows us to create a data structure to calculate and persist the 
##inverse of a matrix for us to retrieve later without have to recalculate it
## example:
## myMatrix <-matrix(rnorm(25),5,5)
## cache<-makeCacheMatrix(myMatrix)
## cacheSolve(cache)


## makeCacheMatrix returns a vector which allows us to 
## 1. set an input matrix (and null'ing any inverse matrix stored to clear the cached result from a previous operation)
## 2. get the input matrix
## 3. set the inverse of that matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  ##this variable, i, holds our inverse matrix when set
  i <- NULL
  
  ##set the matrix (and set i to null so that we reset the inverse)
  setInputMatrix <- function(y){
    x <<- y
    i <<- NULL
  }
  
  ##get (return) the matrix so we can see what it looks like
  getInputMatrix <- function() x
  
  ##set the inverse, i, of the matrix 
  setInverse <- function(inverse) i <<- inverse
  
  ##get (return) the inverse so we can see it
  getInverse <- function() i
  
  list(setInputMatrix = setInputMatrix, 
       getInputMatrix = getInputMatrix, 
       setInverse = setInverse, 
       getInverse = getInverse) 
}


## cacheSolve will take a vector from makeCacheMatrix and calculate the inverse of that matrix 
## unless it has already been calculated in which case it will just return the inverse cached in the
## vector

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## call getInverse to see if we have already calculated the inverse of x
  i <- x$getInverse()
  
  ## if i is not NULL then we have already calc'd and cached the answer so we can just return it
  ## we can also give a message to let us know we have a cache hit
  if(!is.null(i)){
    message("getting cached data...")
    return(i)
  }
  
  ## if i was NULL then we have to calculate the inverse of x and then can store it for use later so we:
  ## get the input matrix
  myMatrix <- x$getInputMatrix()
  
  ## invert it
  i <- solve(myMatrix, ...)
  
  ## and then stick it in the cache
  x$setInverse(i)
  
  i
}
