#creates a special matrix object that can cache its inverse
#First, convert makeChacheMatrix to a function of argument Matrix
makeCacheMatrix <- function(x = matrix()) {
  #create object i that will store the inverse. Null implies that it is yet to be identified
  i <- NULL
  #set the value of the matrix
  set <- function(data) {
    x <<- data ##caches the inputted matrix so that cacheSolve can check whether it has changed
    i <<- NULL ##set the value of i to Null if cacheSolve was used
  }
  #return the matrix x
  get <- function() x
  #sets the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  #return the per-set inverse of the matrix
  getinverse <- function() i
  #Return the matrix with our newly defined functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x=matrix(), ...) {
  # get the inverse of matrix if it was previously calculated
  i <- x$getinverse()
  #check to see if cacheSolve was run before
  if(!is.null(i)) {
    #print out the following message if inverse was pre set
    message("getting cached data")
    #return cached data
    return(i)
  }
  #if inverse is not cached, run the get function to return matrix
  data <- x$get()
  #calculate inverse
  i <- solve(data, ...)
  #set calculated inverse
  x$setinverse(i)
  #return calculated inverse
  i
}

#create inverse and assign to x
x <- matrix(rnorm(16), nrow = 4, ncol=4)
#makeCacheMatrix x; i.e. create special matrix
cx <- makeCacheMatrix(x)
#return the matrix
cx$get()
#solve for inverse
cacheSolve(cx)
#return cached inverse
cacheSolve(cx)       
