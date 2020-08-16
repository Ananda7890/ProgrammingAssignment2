## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function 
## There are two functions. They are makeCacheMatrix,cacheSolve
##makeCacheMatrix consists of set,get,setinv,getinv
##makeCacheMatrix function creates a special matrix object that can cache its inverse
##cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL  ##initializing inverse as NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function()x ##funtion to get matrix
    setinverse<-function(inverse)inv<<-inverse
    getinverse<-function()inv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { ##getting cache data
    inv<-x$getinverse()  ## Return a matrix that is the inverse of 'x'
    if(!is.null(inv)){ ##checking whether inverse is null
        message("getting cache data")
        return(inv) ##return inverse value
    }
    mat<-x$get()
    inv<-solve(mat, ...) ##calculating inverse value
    x$setinverse(inv)
    inv ##returns a matrix that is inverse of 'x'
}
