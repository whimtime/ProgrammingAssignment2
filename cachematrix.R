
## this function cache the value of inverse matrix
makeCacheMatrix <- function(x = matrix()) 
{
  # Initialize global value to NULL
  cacheValue <- NULL
  
  # create the set function with the <<- operator to assign value to cacheValue (different environment)
  set <- function(y) {
    x <<- y
    cacheValue <<- NULL
  }
  
  # create the get function 
  get <- function() x
  # create the setMatrix function to set the value to chached matrix
  setMatrix <- function(inverse) cacheValue <<- inverse
  # get the inverted matrix from cache
  getInverse <- function() cacheValue
  
  # return the created functions to the working environment
  list(set = set, get = get, setMatrix = setMatrix,getInverse = getInverse)
    
}

# this function calculate the inverse of matrix

cacheSolve <- function(x, ...) 
{
  ## assign of the inverse matrix in the global variable
  cacheValue <- x$getInverse()
  
  # check if the cached value exists (singleton like), if exists return the global variable
  #the function break
  if (!is.null(cacheValue)) {
    return(cacheValue)
  }
  
  # create matrix since it does not exist
  matrix <- x$get()
 
    # set and return inverse of matrix
    cacheValue <- solve(matrix, ...)
    #set the value to the cached variable
    x$setMatrix(cacheValue)
   
  return (cacheValue)
}
