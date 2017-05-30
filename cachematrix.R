## First Peer graded assignment
## 

##  This function creates a special "matrix" object that can cache its inverse.
##  We are trying to stay close to the provided example :

##  1- set the matrix value
##  2- get the matrix value
##  3- set the inverse matrix
##  4- get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## First part should be the same as the example , just use inverse in stead of m
    inverse <- NULL
    set <- function(y) {
    x <<- y
    inverse <<- NULL 
    }
  
 ## Get  the function
  get <- function() x

 ## Now set and get thje inverse
  set_inverse <- function(inverse) inverse <<- inverse
  get_inverse <- function() inverse
  
 ## Make the list
  list(set = set, 
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## again stay close to the example
  inverse <- x$get_inverse()
  
  ## And let see if we already have something
  if(!is.null(inverse)) {
    # just show the data
    message ("We have it already in cache")
    return(inverse)
  }
  
  ## if not we will make the inverse
  matrix_data <- x$get()
  inverse <- solve(matrix_data, ...)
  
  # We have it now,  let's put it in cache
  x$set_inverse(inverse)
  
  ## And we want to see it too
  inverse
 }
