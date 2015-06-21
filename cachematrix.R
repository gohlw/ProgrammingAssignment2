## R programming week3 assignment 2

## Thank you for your time for reviewing this assignment.
## please find below a Test case that used to test the function.

##Test case :
## 1.) create a matrix, eg: a <- matrix(1:4,nrow=2)
## 2.) Run for the first time, eg: 
##     x <- makeCacheMatrix(a)
##     cacheSolve(x)
##
##     output:
##     no cache found, processing...
##          [,1] [,2]
##     [1,]   -2  1.5
##     [2,]    1 -0.5
##
## 3.) Run for second time (cached) with same matrix define on step 1.), eg:
##     cacheSolve(x)
##     
##     output:
##     getting cached data
##          [,1] [,2]
##     [1,]   -2  1.5
##     [2,]    1 -0.5
## 
## 4.) create a new matrix with different value from step 1.), eg: b <- matrix(5:8,nrow=2)
## 5.) overwrite current matrix in x from a to b, eg: x$setCurrentMatrix(b)
## 6.) Run cacheSolve() again, because b is not the same with a, we are expecting "no cache found, processing"
##     cacheSolve(x)
##
##     output:
##     no cache found, processing...
##     [,1] [,2]
##     [1,]   -4  3.5
##     [2,]    3 -2.5


## This function creates a special "matrix" object that can cache its inverse
## Note : Think of it like java bean object
makeCacheMatrix <- function(x = matrix()) {
  
  ##cached matrix value, default to NULL so it can be identify 
  ##by using !is.null later, if it is not null, it has been overwritten, thus, cached.
  inversed_matrix <- NULL
  cached_matrix <- NULL ##store the matrix as-is without inverse
  
  ##getter of current passed in x value
  getCurrentMatrix <- function() x
  setCurrentMatrix <- function(cm) x <<- cm
  
  ##getter setter of cached inverse matrix
  setCachedInverseMatrix <- function(i_matrix) inversed_matrix <<- i_matrix
  getCachedInverseMatrix <- function() inversed_matrix
  
  ##getter setter of matrix, for comparison purpose
  ##Quote from assignment : 
  ##"(and the matrix has not changed)..."
  getCachedMatrix <- function() cached_matrix
  setCachedMatrix <- function(matrix) cached_matrix <<- matrix

  ##return all the above as a list (of functions) so it can be fetch
  ##else "object of type 'closure' is not subsettable" will occur =p
  list(getCurrentMatrix=getCurrentMatrix,
       setCurrentMatrix=setCurrentMatrix,
       setCachedInverseMatrix=setCachedInverseMatrix,
       getCachedInverseMatrix=getCachedInverseMatrix,
       getCachedMatrix=getCachedMatrix,
       setCachedMatrix=setCachedMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## if it is already initialized on makeCacheMatrix() and current matrix is the same with cached matrix, return the stored matrix
## else process it, and overwrite the cached matrix and cached inverse matrix
cacheSolve <- function(x, ...) {
  i_m <- x$getCachedInverseMatrix() ##cached inverse matrix
  m <- x$getCachedMatrix() ##the last matrix that pass in, that used to calculate cached inverse matrix above
  data <- x$getCurrentMatrix()
  
  ## If current passed in matrix is same with previous cached matrix
  ## Quote Assignment agenda : "(and the matrix has not changed)"
  if(!is.null(m)){
    ##If the inverse has already been calculated
    if(m==data && !is.null(i_m)) {
      message("getting cached data")
      return(i_m)
    }
  }
  
  #If not fitting the condition above
  message("no cache found, processing...")
  i_m <- solve(data)
  ##replace previous cached matrix with the matrix that use to generate the inverse
  x$setCachedMatrix(data)
  ##replace previous inversed matrix
  x$setCachedInverseMatrix(i_m) 
  i_m
    
}
