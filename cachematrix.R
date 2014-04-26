
## The following function, makeCacheMatrix() creates a special "matrix", 
## which is really a list containing functions to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

## the argument for this function is a matrix
## The list returned will also contain the pointers to the environments
## where the values are stored

makeCacheMatrix <- function(x = matrix()) {
m <- NULL                         			## NULL is assigned to m
    set <- function(y) {              		## the set function can be used to assign any
        x <<- y                       		## matrix (y) to x and NULL to m and may be used 
        m <<- NULL                   		## for testing our function
    }
    get <- function() x               		## the get function simply return the matrix x
    setinverse <- function(inv) m <<- inv  	## the setinverse function retrieves the inverse matrix that is passed as an argument (from the cacheSolve() 
											## function below and stores it in m 
    getinverse <- function() m				## the getinverse() function, simply returns the inverse matrix as stored in m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The following function cacheSolve(), takes the x list returned by the makeCacheMatrix function and checks to see if the inverse of this matrix 
## already exists (using the getinverse function of x). If it does, then the cached inverse matrix is returned otherwise, the actual matrix 
## that was passed as an argument to the makeCacheMatrix function above is retrieved (using the get function of x) and its inverse is calculated 
## using the solve() function. The setinverse() function of x is used to set the newly computed inverse matrix in the cache.


cacheSolve <- function(x, ...) {
    m <- x$getinverse()  		## the getinverse function queries the cache of x and either returns a NULL or already cached valued of the inverse
    if(!is.null(m)) {     		## if the cache already exists, the condition will be true
        message("getting cached data")
        return(m)         		## and the cache will be returned
    }
    data <- x$get()     		## if there is no cache, then the matrix x that was passed to the 
								## functionmakeCacheMatrix (above) will be returned and moved to "data" 
    m <- solve(data,...) 		## the solve function (built in R) computes the inverse of the matrix
    x$setinverse(m) 			## and sets the newly computed mean in the cache of x
    m                 			## the result is returned here
} 