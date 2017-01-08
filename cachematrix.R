## Program to describe the working of lexical scoping and caching in R. 
## Function "makeCacheMatrix" used to set and get matrix inverse from cache if exists
## Function "cacheSolve" used an object of "makeCacheMatrix" for inverse computation.

## "makeCacheMatrix" function caches inverse of a matrix called repeatedly
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  ##get the dimensions of the matrix to find matrix is invertible or not
  ##inverse <- matrix(0,nrow=dim(x)[1],ncol=dim(x)[2]) 
  ##setter for matrix x
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  #return the list containing functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##"cacheSolve" function uses the existing "x" object to get the inverse of
##matrix or set the matrix inverse if not exists..
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    print('inverse exists')
    return(inverse)
  }
  print('inverse not exists')
  #set the inverse of the matrix
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}