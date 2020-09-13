## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates an R object that is a list of 4 functions. It also includes two data objects
## x and im.  The four functions set the value of the matrix, get the value of the vector
## works out the value of the inverse and get the value of the im

makeCacheMatrix <- function(x = matrix()) {
  
  im <- NULL   
  
  set <- function(y) {     
    x <<- y
    im <<- NULL
    return(y)
  }
  
  get <- function() x      
  setim <- function(solve) im <<- solve    
  getim <- function() im                
  
  list(set = set, get = get,   
       setim = setim,
       getim = getim)
}

## Write a short comment describing this function
## if im (inverse matrix) is NOT null it will print message 'getting chached data' and print the cached im.
## This only happens if you have run already. If it is null it has not been calcualtated yet 
## and the solve functoin is carried out on your matrix and a new im is returned.

cacheSolve <- function(x, ...) {    
  im <- x$getim()  
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()  
  im <- solve(data, ...)  
  x$setim(im)    
  im
}

## TO TEST
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
myMatrix <- makeCacheMatrix(m1)
cacheSolve(myMatrix)  