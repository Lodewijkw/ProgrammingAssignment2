#MakeCacheeMatrix and cachesolve functions by Lodewijk Wennemers 10-2017

#MakeCacheMatrix function, only accepts arguments of class matrix. Function creates the special matrix object
MakeCacheMatrix <- function(x = matrix()) { 
  #set m to  NULL to use it later
  m <- NULL
  #create the set function which assigns the value y to x and NULL to M in the parent environment of the function set
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get function to "print" the value of x
  get <- function() x
  #set the value of m in the parent of the setmatrix function to the value of b
  setmatrix <- function(b) m <<- b
  #"print" matrix stored in m
  getmatrix <- function() m
  # create a list with the functions above and name them accordingly
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}
#Cachesolve function, calculates the inverse of the matrix object or retrieves its value from cache when already calculated.
cachesolve <- function(x, ...) {
  #assigns the value of x$getmatrix to m
  m <- x$getmatrix()
  #if there was a solved matrix stored previously, in x, print a message and the solved matrix and end the cachesolve function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if m was NULL in the previous statement then data(matrix) gets the value of the get function in the special matrix object
  data <- x$get()
  #the inverse for the input matrix is stored in m
  m <- solve(data, ...)
  #the solved matrix is set to the special matrix object
  x$setmatrix(m)
  # the solved matrix is printed
  m
}
