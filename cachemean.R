## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## https://github.com/maddalir/ProgrammingAssignment2

## Testing

## Due April 26
## This function creates a special "matrix" object that can cache its inverse.
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##   then cacheSolve should retrieve the inverse from the cache.
cachemean <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data for matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}
