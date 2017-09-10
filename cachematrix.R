## cachematrix.R - 2017Sept09 - M Wagshul
## Following code is designed to calculate the inverse of a matrix
## and store the result in the cache for later recall
## This will greatly speed up subsequent caculations, assuming
## the matrix does not change, so that inverse doesn't need to be recalculated

## makeCacheMatrix
## input - matrix
## defines the matrix object which will contain 4 subfunctions
##    set - set the input matrix, note this call will null the value of the 
##          inverse until it's recalculated
##    get - return the input matrix
##    setInv - set the value of the inverse
##    getInv - return the inverse of the matrix
makeCacheMatrix <- function(Matrix1 = numeric()) {
  theInverse <- NULL
  set <- function(Matrix2) {
    # set the matrix, set inverse to null (until recalculated)
    Matrix1 <<- Matrix2
    theInverse <<- NULL
  }
  get <- function() Matrix1  # spit out matrix to console
  setInv <- function(inputInverse) theInverse <<- inputInverse  # set the inverse
  getInv <- function() theInverse  # output the inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve
## input - matrix object, created in makeCacheMatrix
## This function will take as an argument the object created in makeCacheMatrix,
##  will calculate it's inverse, and place the result into the cache
## If already calculated, will simply return value from the cache

cacheSolve <- function(Matrix1, ...) {
  theInverse <- Matrix1$getInv()
  if(!is.null(theInverse)) {
    message("getting cached data")
    return(theInverse)
  }
  dataMatrix <- Matrix1$get()
  theInverse <- solve(dataMatrix, ...) # calculate the inverses
  Matrix1$setInv(theInverse) # set value of the inverse in the object
  theInverse  # spit out the inverse to console
}
