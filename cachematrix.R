########################################################################
## Matrix inversion is a costly computation  
## Detailed below are two functions that cache the inverse of a matrix  

########################################################################
## Function: makeCacheMatrix(x = matrix())
## Input: standard matrix
## Allows caching of the inverse of the matrix()
## Output matrix with cached inverse option

#########################################################################
## Function: cacheSolve(x)
## Input: Special "matrix" from makeCacheMatrix function 
## Calculates the inverse of the makeCacheMatrix & stores in cache
## Not calculated if inverse is already populated & matrix not changed



# makeCacheMatrix: Creates a special "matrix" object.
# The object returned can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
		
		# Inverse matrix m
		m <- NULL
		# Set matrix & resets inverse
		set <- function(y) {
			x <<- y
			m <<- NULL
		}
		# Get matrix x
		get <- function() x
		# Get & set inverse functions
		setinverse <- function(inverse) m <<- inverse
		getinverse <- function() m
		# Creates list of functions
		list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}

# cacheSolve: Computes inverse of makeCacheMatrix "matrix".
# Get inverse from cache if set or calculate it.
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	# retrieve the inverse from the cache
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	# Calculate inverse
	m <- solve(data)
	# Set inverse in object
	x$setinverse(m)
	m
}


# # Testing & usage
# q <- matrix(c(2, 0, 0, 2), nrow = 2, ncol = 2, byrow = TRUE)
# q
# # Create a special matrix
# c <- makeCacheMatrix(q)
# c$get()
  
# c1 <- solve(q)
# # Set inverse
# c$setinverse(c1)
# c$getinverse()
  
# # Get cache inverse
# cacheSolve(c)
  
  
# ## Calculate inverse
# m <- matrix(c(3, 0, 0, 4), nrow = 2, ncol = 2, byrow = TRUE)
# m
# c$set(m)
# c$getinverse()

# cacheSolve(c)  


