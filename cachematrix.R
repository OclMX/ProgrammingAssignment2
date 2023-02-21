## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The Cache Matrix function will provide "helper methods" to set and get both the
## matrix and inverse matrix to support cacheSolve to limit compute resources.
## The function will return a list functions to set or get a matrix and an inversed matrix.


makeCacheMatrix <- function(x = matrix()) {
  
  ##create and initialize the variable for the inverse
  inverseMatrix <- NULL
  
  ##set the matrix
  set <- function(x){
    ##superassignment to start with this environment
    
      x <<- x
      inverseMatrix <<- NULL
  }
  ##get the matrix
  get <- function(){
    ##Return called explicitly
    return(x)
  }
  
  ## set the inverse of the matrix
  setInverse <- function(iMatrix){
    inverseMatrix <<- iMatrix
    
  }
  
  ## get the inverse of the matrix
  getInverse <- function(){
    
    return(inverseMatrix)
  }
  
  ## This list of functions that will allow for an easy way to manage
  ## the matrix and it's inverse.
  ## Renamed the set and get to a more intuitive set of names.
  
  list (setMatrix = set, getMatrix = get, 
        setInverse = setInverse, getInverse = getInverse )
  

}


## Write a short comment describing this function
## Using the wrapper/helper functions of the makeCacheMatrix funciton
## determines if there is a cached inversed matrix. If there is, then return 
## the cache Otherwise take the new matrix and inverse it using the 
## R solve function. Completing the work by "caching" the 
## inversed matrix and then returning it. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
 
  inverseMatrix <- x$getInverse()
  ## Just realized this is not type safe
  
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }
  
  ## x representing the parameter of the makeCacheMatrix function
  ## Matrix representing the data to be inversed.
  ## Just getting the matrix to pass to the solve function.
  
  xMatrix <- x$getMatrix()
  
  
  ## pass the matrix to the solve function
  ## per documentation solve for a square matrix, but assume
  ## it is always invertible
  
  inverseMatrix <- solve(xMatrix) 
  
  ##set the inverse to have it cached for later use
  x$setInverse(inverseMatrix)
  
  ## return the results
  inverseMatrix
  
}
