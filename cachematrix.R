## Caching inverse of a matrix.
## Call makeCacheMatrix with the matrix whose inverse is to be cached as a parameter to create a special "matrix" object
## Call makeCacheMatrix with the above created "matrix" object

## Creates a "matrix" object which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
  
    	set <- function(y) {
        		x <<- y
	    		inv <<- NULL
	      	}
	        
	get <- function() x
		    
	setinverse <- function(inverse) { 
				inv <<- inverse
			}

	getinverse <- function() inv
				  
	m<-matrix(c(set, get, setinverse, getinverse), 2, 2)
				    
	dimnames(m)<-list(c("set", "get"), c("original", "inverse"))

	m
}


## Computes the inverse of special "matrix" created with makeCacheMatrix
cacheSolve <- function(x, ...) {
        inv <- x[["get", "inverse"]]()
	
	if(!is.null(inv)) {
	    message("get cached data")
	    return(inv)
	}
	
	data <- x[["get","original"]]()
	
	inv <- solve(data)
	
	x[["set","inverse"]](inv)
	
	inv
}
