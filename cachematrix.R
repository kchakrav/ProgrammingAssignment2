##The first function, `makeCacheMatrix` creates a special "Matrix", which is
##really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the matrix
##4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {  
	m<-NULL	
	set<-function (y) {
		x<<- y
		m<<- NULL
	}
	get<-function () x
	
	setMatrix <- function(mat) m<<-mat
	getMatrix <- function() m
	
	list(set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)	
}

#The second function computes the inverse of a matrix. It first checks if the inverse of the matrix is already calculated. 
#If yes, it returns the results from the cache else it will calculate the inverse of the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m<-x$getMatrix()
		if (!is.null(m)) {
			message("getting cached data")
			return(m)
		}
		data<-x$get()
		m<-solve(data)
		x$setMatrix(m)
		m		
}