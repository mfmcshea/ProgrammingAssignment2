##R Programming Class
##Assignment 2
##Program Name: cachematrix.R
##Date: August 20, 2015, 4:20PM

#set working directory
setwd('C:/Users/McShea/Desktop/DataScience/ProgrammingAssignment2')

## Put comments here that give an overall description of what your
## functions do:
## The purpose of this assignment is to write a pair of functions
## that cache the inverse of a matrix.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## This function is a list for
## 1. setting the value of the matrix
## 2. getting the value of the matrix
## 3. setting the value of the inverse
## 4. getting the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.  If the inverse has
## already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  x$setinv(inv)
  inv
}

##Checking out whether the matrix inversion works
A=matrix(c(1,-0.25, -0.25, 1), nrow=2, ncol=2,byrow=TRUE)
A
solve(A)%*%A
##Get an identity matrix.

##Calculate matrix inversions using both methods.
## Let mat = invertible matrix
dotheywork = function(mat) {
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  time.elapsed = Sys.time() - start.time
  print(time.elapsed)
  
  start.time = Sys.time()
  cacheSolve(temp)
  time.elapsed = Sys.time() - start.time
  print(time.elapsed)
}

dotheywork(A)
#Time difference of 0 secs
#getting cached data
#Time difference of 0 secs

#So it seems like the time difference in inverting 2x2 matrices is trivial.  
#Instead, explore the time difference in inverting a 1000 by 1000 matrix 
#of random numbers.

set.seed(1234) #Set the seed.  We want to be able to exactly replicate this exercise.
random.matrix = rnorm(1000000) #Populate a matrix with 1000 * 1000 = 1M entries
mat1 = matrix(random.matrix, nrow=1000, ncol=1000) #Let mat1 equal this matrix
dotheywork(mat1) #Run mat1 through dotheywork() and compare the time differences

#Time difference of 2.661152 secs
#getting cached data
#Time difference of 0.0009999275 secs