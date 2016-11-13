## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix puts a matrix into cache or returns it from cache.

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x <<- y

	}
	get <- function() x
	setinv<-function(inverse) inv<<- inverse
	getinv<-function() inv 
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## Shakespeare's favorite.  Checks if inverse matrix of x exists in cache.  If
## not, computes it and places it in cache.  Also prints to console.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <-x$getinv()
 	if(!is.null(inverse)){
		message("getting cached matrix")
		return(inverse)
	}
	mat <-x$get()
	inverse<-solve(mat)
	x$setinv(inverse)
	inverse
}


#Small Test Script: generates matrix and tests functions
set.seed(999)
z<-matrix(rnorm(9),3,3,byrow=TRUE)
x<-makeCacheMatrix()
x$set(z)
a<-cacheSolve(x)
x$getinv()




