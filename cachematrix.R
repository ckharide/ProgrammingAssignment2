# Functions to build and compute the inverse of a matrix are makeCacheMatrix and cacheSolve. 

# makeCacheMatrix creates a list with the following functions. 
#  set/gets the value on  matrix
#  setinverse/getinverse the value of inverse of the matrix
#  amatrix = makeCacheMatrix(matrix(c(4,1,3,2), nrow=2, ncol=2)) will create a matrix of 2 Rows and 2 Columns 


makeCacheMatrix <- function(x = matrix()) {
	m <-NULL
	#set the matrix with new data. 
	set <- function(y) 
	{
		x <- y
		m <<- NULL
	}
	
	# Returns the matrix
	get <- function()  {  x }
	
	#  set the inverse on the matrix 
	setinverse <- function(solve)  { m <<- solve } 
	
	# Gets the inverse from the matrix
	getinverse <- function() { m }
	
	
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)

}


# Computes the inverse of matrix, if its already there returns from cache  
# otherwise  compute the inverse and store the result in Cache. 

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inversevalue <- x$getinverse()
	
	
	if(!is.null(inversevalue)) {
			message("getting inverse from Cache ")
			return (inversevalue)
	}	
	data <- x$get()
		
	#compute the inverse 
	inversevalue <- solve(data)
	
	# store the inverse.
	x$setinverse(inversevalue)
	inversevalue
}
