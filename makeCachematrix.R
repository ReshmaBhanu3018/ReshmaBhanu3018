makeCachematrix <- function(x= matrix)){
        inv <- NULL
        set <- function(y){
          x<<-y
          inv<<-NULL
        }
        get<-function() {x}
        setInverse <_ function(inverse) {inv<<-inverse}
        getInverse <-function() {inv}
        list(set=set,get=get,setinverse = setinverse,getInverse= getInverse)
}
cachesolve<- function(x,.....){
    inv<-x$getInverse()
    if(!is.null(inv)){
      message("getting cached date")
      return(inv)
    }
    mat<-x$get()
    inv<-solve(mat,....)
    x$setinverse(inv)
    inv
}