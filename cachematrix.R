## CACHING THE INVERSE OF A MATRIX
## Below functions will cache the inverse of a matrix which is useful when 
## the said inverse of the matrix is needed from time-to-time within the program.

## First function will help us create an object containing a list of functions
## that will create the special "matrix" and will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL              ## created a null object "i" where the inverse is going to be stored
  set <- function(y) {   ## stored the matrix "x" in object "set"
    x <<- y
    i <<- NULL
  }
  get <- function() x                                ## get the value of the matrix
  set_inverse <- function(inverse) i <<- inverse     ## set the value of the inverse
  get_inverse <- function() i                        ## get the value of the inverse
  list(set = set, get = get,                         ## simplify all the functions in a list
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
}

## Second function will compute the inverse of the matrix inputted in the function
## (makeCacheMatrix) above. If the contents of the matrix has not changed and has been 
## already calculated, it should get the cached inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$get_inverse()                  ## get the inverse stored using the above function
  if(!is.null(i)) {                     ## to determine if the inverse has been calculated
    message("getting cached data")      ## message "getting cached data" will flashed if the
    return(i)                                  ## inverse has been calculated before for the
  }                                            ## given matrix. afterwards, it will return the
  m <- x$get()                                 ## inverse
  i <- solve(m, ...)                    ## else it will solve the inverse
  x$set_inverse(i)
  i
}




