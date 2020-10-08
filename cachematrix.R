## Put comments here that give an overall description of what your
## functions do

# The first function, makeCacheMatrix() creates an R object that stores a 
# matrix and its inverse. 
# The second function, cacheSolve(), if the inverse has already been 
# calculated (and the matrix has not changed), uses an argument, 
# returned by makeCacheMatrix(), to retrieve the inverse from the cached
# value that is stored in the makeCacheMatrix() object's environment.
# The second function can retrieved these objects because the entire 
# makeCacheMatrix() environment stays in memory can be accessed through pointers.


## Write a short comment describing this function

  # This function allows to create an object that contains four functions:
  # set(), get(), setinverse(), and getinverse(). 
  # It also includes the two data objects, x and inv.
  # It builds a set of functions and returns the functions within a list 
  # to the parent environment. 


makeCacheMatrix <- function(x = matrix()) {  # This function allows to create an object that contains four functions: set(), get(), setinverse(), and getinverse(). It also includes the two data objects, x and inv.
  inv <- NULL  # variable inv is initialized to NULL. x was initialized as a function argument.
  set <- function(y) { # the set function is defined with the input argument y
    x <<- y   #  y (input argument) is assigned to the x object in the parent environment
    inv <<- NULL # the value NULL is assigned to inv object in the parent environment. Any value of inv that had been cached previously is cleared.
  }
  get <- function() x # the getter for matrix x is defined. x will be retrieved from the parent environment of makeCacheMatrix()
  setinverse <- function(solve) inv <<- solve # the setter for the inverse inv is defined. The assignment operator form <<- assigns the input argument (solve) to the value of inv in the parent environment
  
  getinverse <- function() inv # the getter for the inverse inv is defined
  list(set = set, get = get,  #  Each of the functions (set, get, setinverse, getinverse) is assigned as an element within a list(), and returns it to the parent environment and each element of the list is named.  
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
 # cacheSolve() is required to populate and/or retrieve the inverse 
 # from an object of type makeCacheMatrix().
 # cacheSolve() function can access the values of the 
 # matrix and its inverse defined in the environment of 
 # the original function.
 
 
 
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) { #x is a formal argument of the function.  In this case, it should be the object where the output of makeCacheMatrix is stored. 
  inv <- x$getinverse()  # getmean() function on the input object is called.
  if(!is.null(inv)) { # if the previous step did not assign a NULL value, it will return the cached data to the parent environment and print a message   
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get() # if the previous step assigned a NULL value to inv then  cacheSolve gets the vector from the input object
  inv <- solve(data, ...)# cacheSolve calculates a mean() 
  x$setinverse(inv) # the setmean() function is used on the input object to set de inverse matrix
  inv # the value of the inverse matrix is printed
}