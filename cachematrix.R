## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # x represents our data frama, which is a matrx 
  
  
  m <- NULL
  # m is a null object, its going to takes the values of the inverse
  
  # the function is used to set the matrix
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  
  # Method to get the matrix
  get <- function() x
  
  
  setInverse <- function(inverse) m <<- inverse
  #use the <<- operator, allows me to save the object in a diferente workspace
  # the object setInverse its going to cache the inverse
  
  # Get inverse we can get the inverse, after calculate it
  getInverse <- function() m
  
  # its a list of all the elements in makeCacheMatrix Function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
# once we use the makeCacheMatrix, we will have an object with 4 elements


#Cachesolve
cacheSolve <- function(x, ...) {
  #cache solve is going to use the object X before defined
  
  m <- x$getInverse() # we save the object "getinverse" with other name
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }  # if the object "x$geatinverse" its null, its give me a warning
  # if not, give me the object 
  
  data <- x$get() # we takes the elements X and store wiht the na,e data
  m <- solve(data, ...)
  x$setInverse(m) #save the object m, which is the inverse in the null vector m
  m # give me the inverse
}

cinverse<- makeCacheMatrix(matrix(c(1:4),nrow=2, ncol=2)) 
# using the makeCacheMatrix, i built a objecte with the 4 elements

cinverse$get() # i can see it 

cinverse$getInverse() # we get a null vector because at the time we didnt 

cacheSolve(cinverse) # now we can see that we can solve the matrix

cinverse$getInverse() #the inverse its store in the makeCacheMatrix 

