## These are two functions that cache a matrix inverse 
## If the inverse is not yet calculated, it is calculated by the functions
## If it has been calculated, the result is returned (and printed)



## This function creates a set of functions and puts them into a list

makeCacheMatrix <- function(x = matrix()) {
  ## i is defined as null and a funtion 'set' is defined that again defines i as null and sets x as a matrix y
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## more functions are defined: get - getting matrix x, 
  ## seti - setting i as the matrix defined by the argument of the function, geti - getting the i
  get <- function() x
  seti <- function(inv) i <<- inv
  geti <- function() i
  ## a list with functions is defined
  list(set = set, get = get,
       seti = seti,
       geti = geti)
}

## This function gets the matrix inverse, if it exists;
## if it doesn't exist, the function calculates the inverse and prints it

cacheSolve<- function(x, ...) {
  ## this part gets the matrix i (that is supposed to contain the inverse) 
  ## and checks if it is not null; if it is not null, the content is printed after the message 'getting cached data'
  i <- x$geti()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if i is null, the initial matrix is sourced and its inverse is calculated and printed
  data <- x$get()
  i <- solve(data)
  x$seti(i)
  i
}