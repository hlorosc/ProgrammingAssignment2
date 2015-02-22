##makeCacheMatrix stores the value of the matrix entered into the function 
  ##and initiates a vector that will be used to store the inverse matrix called getSolve
  
  makeCacheMatrix <- function(x = matrix()){
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get, setSolve = setSolve,getSolve = getSolve)}

##cacheSolve takes the output list of makeCacheVector and calculates the inverse matrix
##for the matrix input into the makeCacheVector function. If the this calculation has
##been completed before then x$getSolve() will already be populated and that data returned
##if x$getSolve is empty then the function calculates the inverse matrix and stores the solution
#in x$setSolve

> cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setSolve(m)
  m }
