## The function makecachematrix and cachesolve together is meant to take an
##invertible matrix type input, calculate it's inverse using r function solve() 
##and save the inverse in cache to be used when the function is called again with
##the same input.

## The function makeCacheMatrix returns four functions to the parent environment 
## as a list of named elements along with all objects created in the functions.
## It uses the <<- operator to retain the inversed matrix in s and return it 
## when function CacheSolve is called with the same input. When input is changes 
## S is set to null forcing cache solve to recalculate inverse.


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


## Function calculated the inverse of matrix input to makeCacheMatrix
## takes a makeCasheSolve type list as input and call its elements .First it calls
## getsolve, check if there is any inverse data in cache. The environment
## defining getsolve has already set s to null, hence first time calls for an 
## input  results to false . So function calls to get() functions receive the X value 
## as input matrix calculated inverse and stores in cache using setsolve.

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
