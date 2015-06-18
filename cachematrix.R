## With these functions one can create a special matrix with 
## the ability to cache its inverse. 
## Instead of recomputing the inverse each time it is needed, 
## the inverse can be computed just once and then cached. 
## The cached value will be retreived afterwards.

## This function creates the spcial matrix, with the ability to cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
	x.inv <<- NULL
	
	set <- function(y){
		x <<- y
		x.inv <<- NULL
	}
	get <- function(){x}
	
	getinverse <- function(){x.inv}
	
	setinverse <- function(inv){
		x.inv <<- inv
	}
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## This function retreives inverse of the matrix if a cached value exists.
## Otherwise, it computes the inverse and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        
        d <- x$get()
        if(dim(d)[1]==dim(d)[2] && !is.na(dim(d)[1])){
        	b <- diag(dim(d)[1])
        	# I intentionally send the identity matrix as the 
        	# second parameter to make sure solve is used only 
        	# for computing the inverse
        	inv <- solve(d,b, ...)
        	
        	x$setinverse(inv)
        	return(inv)
        }
}
