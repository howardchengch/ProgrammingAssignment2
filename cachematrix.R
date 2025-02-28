## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inver) inver <<- inver
  getInverse <- function() inver
  
  return(list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse))
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(class(x) == "list"){
    # Note, it uses list element's name to access the element
    inver <- x$getInverse()
    
    if(is.null(inver)){
      inver <- solve(x$get(),...)
      x$setInverse(inver)
    }
    
    return(x$getInverse())
    
  }
  else
    stop("Invalid class: ", class(x))
}