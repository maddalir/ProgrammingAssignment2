## Two functions that cache a square matrix and its computed inverse
## 

## makeCacehMatrix - creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #var to hold calculated inverse of matrix
  cachedInverseMatrix <- NULL
  
  #set a new matrix
  setSquareMatrix <- function(y) {
    
    # don't invalidate the cache with a bad matrix
    if ((!is.null(y)) && is.matrix(y) && (dim(y)[1] == dim(y)[2])) {
      # save a calc if the original matrix is null
      if (is.null(x) || !isSameMatrix(x,y))
      {
        x <<- y
        cachedInverseMatrix <<- NULL
        message("resetting cache")
      } else {
        message("existing cache maintained")      
      }
    } else {
      message("invalid matrix - existing cache maintained")
    }
    cachedInverseMatrix
  }
  
  # compare Two matrices
  isSameMatrix <- function (x,y) {
    
    if (all(dim(m1) == dim(m2))) 
    {  
      if (!is.na(x)  && !is.na(y))       
      {
        # validate y is a matrix
        result = all(apply(x==y,1,all))
        return(result)
      }
    }
    FALSE
  }
  
  # return the internal square matrix
  getSquareMatrix <- function() x
  
  # set/create the inverse matrix
  setCachedInverse <- function(computedInverseMatrix) {
    message("setting cachedInverseMatrix")
    cachedInverseMatrix <<- computedInverseMatrix
  }
  
  # get the inverse matrix
  getCachedInverse <- function() cachedInverseMatrix 
  
  # list public functions
  list (set = setSquareMatrix, get = getSquareMatrix,
        setInverse = setCachedInverse,
        getInverse = getCachedInverse)
}

## cacheSolve - compute or retreive the inverse of an existing square matrix
cacheSolve <- function(x, ...) {
  # get the inverse
  inverseMatrix <- x$getInverse()
  if (!is.null(inverseMatrix)) {
    message("getting cached inverse matrix")
    return(inverseMatrix)
  }
  
  # get square Matrix data
  squareMatrix <- x$get()
  
  
  inverseMatrix <- solve(squareMatrix,...)
  x$setInverse(inverseMatrix)
  inverseMatrix
  
}

