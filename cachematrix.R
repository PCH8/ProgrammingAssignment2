## The function makecachematrix and cachesolve together is meant to take an
##invertible matrix type input, calculate it's inverse using r function solve() 
##and save the inverse in cache to be used when the function is called again with
##the same input.

## The function makeCacheMatrix returns four functions to the parent environment 
## as a list of named elements along with all objects created in the functions.
## It uses the <<- operator to retain the inversed matrix in s and return it 
## when function CacheSolve calls the respective function with the same input. When input matrix changes
## S is set to null forcing cacheSolve to recalculate inverse.


makeCacheMatrix <- function(x = diag(2)) {
  s <- NULL
  set <- function(y) {
    x <<- (y)
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solvedinv) s <<- solvedinv
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function cacheSolve calculates the inverse of matrix that is input to makeCacheMatrix.
## It takes a list of type makecacheMatrix as argument and calls its functions .First it calls
## getsolve, then checks if there is any inverse data in cache. The environment
## defining getsolve has already set s to null, hence first time calls input  results to false . 
## Then cacheSolve calls to get() and receives the X value (input matrix for makeCacheMatrix) 
## then it calculates inverse using solve() and calls setsolve to store result in cache.

cacheSolve <- function(lst, ...) {
  s1 <- lst$getsolve()
  if(!is.null(s1)) {
    message("getting cached data")
    return(s1)
  }
  data <- lst$get()
  s1 <- solve(data, ...)
  lst$setsolve(s1)
  s1
}
