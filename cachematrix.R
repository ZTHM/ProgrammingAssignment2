## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, makeVector creates a special "Matrix", which is really a list containing a function to
##set the value of the Matrix
##get the value of the Matrix
##set the value of the Inverse
##get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
Inv<-NULL
	set<-function(y) 
		  {     x <<-y
		   	Inv <<- NULL
		  }
	get <- function() x
	setx <- function(Inv) Inv <<- inverse
	getx <- function() Inv
	list(set=set,get=get,
		setx=setx,
		getx=getx)  
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 ## Return a matrix that is the inverse of 'x' 
	
       Inv<- x$getx()
	 if(!is.null(Inv)) {
			message("getting Cached Data")
			return (Inv)
	}
      data <-x$get()
	Inv <- solve(data,...)
	x$setx(Inv)
	Inv

}
