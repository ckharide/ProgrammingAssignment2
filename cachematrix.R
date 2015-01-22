# Functions to build and compute the inverse of a matrix are makeCacheMatrix and cacheSolve. 

# makeCacheMatrix creates a list with the following functions. 
#  set -> set the value on  matrix.
#  get -> get the value on  matrix.
#  setinverse -> set the inverse on the matrix.
#  getinverse -> gets the inverse from the matrix.
#  mymatrix = makeCacheMatrix(matrix(c(4,1,3,2), nrow=2, ncol=2)) will create a matrix of 2 Rows and 2 Columns 

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
	
	# Gets the inverse of the matrix
	getinverse <- function() { m }
	
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)

}

# Computes the inverse of matrix, if its already there returns from cache  
# otherwise  compute the inverse and store the result in Cache. 

cacheSolve <- function(x, ...) {

	inversevalue <- x$getinverse()
	
	#Check if the inverse value is in the Cache, if its available return 
	if(!is.null(inversevalue)) {
			message("getting inverse from Cache ")
			return (inversevalue)
	}	
	
	# otherwise compute the inverse
	data <- x$get()
	inversevalue <- solve(data)
	
	# store the inverse.
	x$setinverse(inversevalue)
	inversevalue
}
