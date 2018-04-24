## Matrix Cache function
## The function takes in a matrix as an argument and does the following
## 1 - Set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the inverse of the matrix
## 4 - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  mat <- NULL ## assigns Null value
  
  ## embedded function to set x value of original function
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  
  get <- function() x #get the matrix stored in the function
  setinverse <- function(inverse) mat <<- inverse #store inverse
  getinverse <- function() mat #get the stored inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve function assumes that the matrix is inverted.
## It checks to see if the inverse is calculated and then 
## gets the results, skipping the calculations.
cacheSolve <- function(x, ...) {
  mtx <- x$getinverse()
  if(!is.null(mtx)) {
    message("getting cached data")
    return(mtx)
  }
  data <- x$get()
  mtx <- solve(data, ...)
  x$setinverse(mtx)
  mtx
}
