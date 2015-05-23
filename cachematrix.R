## I have forked the designated remote GitHub repository and
## subsequently cloned it locally in my home directory without
## renaming it, or the files within it  You can see my edit at:

## https://github.com/javierrodrigl/ProgrammingAssignment2.git

## I edited the "cachematrix.R" file in Rstudio.

## In the "makeCacheMatrix" portion of the exercise the goal is
## to create the special "input" matrix for the "cacheSolve" part
## of the assignment.  Specifically, "makeCacheMatrix" will allow
## us to return values of the matrices as needed.

makeCacheMatrix <- function(x = matrix()) {
        inverse_m <- NULL
        define <- function(y) {
                x <<- y
                inverse_m <<- NULL
        }
        ret <- function() x
        define_inv <- function(inverse) inverse_m <<- inverse
        ret_inv <- function() inverse_m
        list(define = define, ret = ret, define_inv = define_inv, ret_inv = ret_inv)
}

## Here, in the "cacheSolve" part of the assignment, we (try to) get
## the inverse as per the instructions with the "makeCacheMatrix"
## part.  In the first place, it looks if the inverse has been
## calculated. If it has, then it gets the "cached" inverse and
## just returns without doing the actual operation. Alternatively,
## it does perform the actual operation and "caches" the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_m <- x$ret_inv()
        if(!is.null(inverse_m)) {
                return(inverse_m)
        }
        vals <- x$ret()
        inverse_m <- solve(vals)
        x$define_inv(inverse_m)
        inverse_m
}