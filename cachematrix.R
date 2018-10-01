## As finding the inverse of a matrix is a costly computation,We will write a function which computes the inverse only once and saves it for fututre reference. The first function creates a matrix and checkes whether the inverse already is calculated. If yes, then it returns the inverse of a matrix, if not we will compute using a function cachesolve.

makeCacheMatrix<-function(x=matrix()){
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	get<-function() x
	setinverse<-function(inverse) inv<<-inverse
	getinverse<-function() inv
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
	}
cacheSolve<-function(x,...){
	inv<-x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data<-x$get()
	inv<-solve(data,...)
	x$setinverse(inv)
	inv
}
