## Cache the inverse of a matrix

##  Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Create a separate enviornment for the inverted matrix variable
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  ## get the value of the matrix x
  get <- function() x
  ## set the value of the inverted matrix x
  set_inv<- function(inverse) m_inv <<-inverse
  ## get the value of the inverted matrix
  get_inv <- function() m_inv
  # Returns the matrix with the functions assigned 
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## Computes the inverse of the special "matrix"  returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  ## Returns the inverted matrix of x
  m_inv <- x$get_inv()
  # Prints the inverse matrix if calculated before
  if (!is.null(m_inv)) {
    message("Inverse matrix of cached matrix")
    return(m_inv)
  }
  # Calculates the inverse if not calculated before
  m_inv <- solve(x$get())
  x$set_inv(m_inv)
  return(m_inv)
}
}
