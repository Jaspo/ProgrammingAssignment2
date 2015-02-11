#makeCacheMatrix will create a object that contains a matrix, it's inverse and a list of functions to get and set those values
#cacheSolve receives the object created before and looks if the inverse is already saved, if it is not, it computes it and it saves it. Anyway it returns the inverse.


##When CacheMatrix is created it's inverse value should be null
##define de function that sets the value of the matrix
##double arrow assignment will modify the variable in the parent level environment
##define de function that gets the value of the matrix
##define de function that sets the cached value of the inverse of the matrix
##define de function that gets the cached value of the inverse of the matrix
makeCacheMatrix <- function (m = matrix()){
  
  
  i <- NULL
  
  
  set <- function(y){
    m <<-y
  }
  
  
  get <-function(){
    return (m)
  }
  
  
  setInverse<-function(inv){
    i<<-inv
  }
  
  
  getInverse<-function() i
  
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##try to get the cached inverse
##if it is not cached, compute it, and cache it 
##if it is cached print a msg
##return the inverse of the matrix
cacheSolve <- function(m, ...){
  
  
  res<-m$getInverse()
  
  
  if (is.null(res)){
    
    res<- solve(m$get(), ...)
    m$setInverse(res)
  }
  else{
    
    message("getting cached data")
  }
  
  
  res 
}