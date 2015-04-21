## These functions implement an object that stores a matrix and computes its
## inverse as required, using a cached version if possible

## Basically, the supplied boilplate is nonsense - way to complicated to use.
## Instead, there should be a single *Matrix* object with setters and getters
## When the inverse is read, the inverse should either be retrieved or computed

## makeCacheMatrix returns an object (list) that implements 4 methods - setters and getters
##  private properties store the matrix and its inverse (computed as necessary)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This returns the inverse of the matrix
## Unfortunately, there is no way to make this function type safe
## If it is passed a vector, it just fails !!

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## This is a better way to do this - single object

##   x<-matrix(1:9,3,3)
##   x[3,3]<-3          ## needed to make x solvable
##   y<-CachedMatrix(x)
##   y$getinverse()

CachedMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {
    x   <<- y
    inv <<- NULL
  }

get <- function() x

## setinverse # not used - inverse is read only

getinverse <- function(){
  if(is.null(inv)) {
    inv <<- solve(x)
  }
  inv
}

list(set = set, get = get,
  getinverse = getinverse)
}

