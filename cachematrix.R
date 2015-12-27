## Put comments here that give an overall description of what your
## functions do

## makeCachedMatrix 

## Write a short comment describing this function
## @x: a square invertible matrix 
## Caches the result in cachedInvMatrix variable
## retuns a list of functions to support following operations:
## 1. get the matrix
## 2. set the matrix
## 3. get inverse
## 4. set inverse
## this list is then used in cacheSolve(...) to get the value from cache if it exists and update the cache if it does not
makeCacheMatrix <- function(x = matrix()) {
  
  ## Cached value
  cachedInvMatrix <- NULL

  ## set the matrix  
  set <- function (inputMatrix) {
    cachedInvMatrix <<- NULL
    x <<- inputMatrix
  }
  
  ## get the matrix
  get <- function() { x }
  
  ## return : the cached inverse matrix
  getCachedInvMatrix <- function() { cachedInvMatrix }
  
  ## update the cached inverse matrix value
  setCachedInvMatrix <- function(invMatrix) {
    cachedInvMatrix <<- invMatrix
  }
  
  ## list holding all the functions to access the cache
  list( set = set, get= get, getCachedInvMatrix = getCachedInvMatrix, setCachedInvMatrix = setCachedInvMatrix)
}


## Write a short comment describing this function
## @x: output of makeCacheMatrix
## Checks if the input contains the cached results
## If not it computes the results using solve() and updates the cache
## else fetches the cache results and retuns it

cacheSolve <- function(x, ...) {
  inverseMatrix = x$getCachedInvMatrix()
  
  if(!is.null(inverseMatrix)) {
    message("Found in cache. Returing cached results")
    return(inverseMatrix)
  }
  
  matrixData = x$get()
  inverseMatrix = solve(matrixData, ...)
  
  x$setCachedInvMatrix(inverseMatrix)
  
  message("Not found in cache. Returing un-cached results")
  return(inverseMatrix)  
  ## Return a matrix that is the inverse of 'x'
}

test <- function() {
  set.seed(12345)
  #r=1:10000
  r=rnorm(100 * 100)
  mat1 = matrix(r, nrow=100, ncol = 100)
  mat2 = makeCacheMatrix(mat1)
  cacheSolve(mat2)
  cacheSolve(mat2)
  cacheSolve(mat2)
  
  r=rnorm(100 * 100)
  mat1 = matrix(r, nrow=100, ncol = 100)
  mat2 = makeCacheMatrix(mat1)
  cacheSolve(mat2)
  cacheSolve(mat2)
  cacheSolve(mat2)
}

# test()