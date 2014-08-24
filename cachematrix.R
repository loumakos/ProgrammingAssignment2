## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function initializes m 
##				then sets the initial values of function
##				it then gets the function
##				The setsolve actualy does the solving
##				with m being assigned the solution
##				the getsolve give out the solution contained in m
makeCacheMatrix <- function(x = matrix()) {
			m<-NULL
			set<-function(y){
				x<<-y
				m<<-NULL}
			get<-function() x
			setsolve<- function(solve) m <<-solve		##Assign m as the solution
			getsolve<-function() m					##This will return m for this matrix when asked
			list(set = set, get = get,
             	setsolve = setsolve,
             	getsolve = getsolve)
}


## Write a short comment describing this function
## The function ask for the solution for this matrix and if it does not exist (m==NULL) it solve it and the caches it
## See inline comments
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()				## get the solution of the inverse into m
        if(!is.null(m)) {				
                message("getting cached data")
                return(m)
        }## here it checks if there is already a sololution for this matrix and returns it
        data <- x$get()			##If there isn't a solution it will ask for the matrix and in the next line assign it to m
        m <- solve(data, ...)
        x$setsolve(m)			##Cache the new solution for this matrix so it doesn't have to be calculated again
        m
}
