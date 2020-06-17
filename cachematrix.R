## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y) {
        x<<-y
        inv<<-NULL
    }
    get<-function(){x}
    set_Inverse<-function(inverse){
        inv<<-inverse
    }
    get_Inverse<- function(){inv}
    list(set = set,get=get,set_Inverse=set_Inverse,get_Inverse=get_Inverse)
         
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    k<- x$get_Inverse()
    if(!is.null(k)){
        message("getting cached inverse of a matrix")
        return(k)
    }
    mat<-x$get()
    k<- solve(mat)
    x$set_Inverse(k)
    k
}
