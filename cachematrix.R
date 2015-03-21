## The 1st. function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  ## Set the value of the special "matrix"
  m <- NULL
  set <- function(y) {
    
    x <<- y
    m <<- NULL
    
  }  
  
  ## Get the value of the special "matrix"
  get <- function() x
  
  ## Set the value of the inverse matrix
  setinvers <- function(solve) m <<- solve
  
  ## Get the value of the inverse matrix
  getinvers <- function() m
  
  ## Define a list with the function names, to pass them into the Global Environment
  list(set = set, get = get,
       setinvers =  setinvers,
       getinvers = getinvers) 
  
}


## The 2nd. function "cacheSolve" computes the inverse of the special "matrix" returned by the function 
## "makeCacheMatrix" from above and checking if there is an existing inverse matrix for x from a pre-run

cacheSolve <- function(x, ...) {
  
  ## Passing the values of the pre-computed "inverse-matrix" as a subset of x to m
  m <- x$getinvers()
  
  ## Check if there is an existing inverse matrix m,
  ## meaning that x has already served as input for makeCacheMatrix
  if(!is.null(m)) {
    
    ## Above condition is true, retrieving the values of the existing inverse matrix
    message("Existing inverse matrix recongnized for this input, getting cached data")
    return(m)
    
  } 
  ## If condition is false, inverse matrix is being calculated
  data <- x$get()
  m <- solve(data)
  ## Saving the results of of the inverse matrix as a subset of x, for the next run
  x$setinvers(m)
  m
  
}