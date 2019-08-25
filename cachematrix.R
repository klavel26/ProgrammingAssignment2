## Put comments here that give an overall description of what your
## functions do

## Functions written for Coursera R programming week 3 assignment 2
## Two Functions - macheCacheMatrix and cacheSolve
## Goal is to work on ability to cache matrix inverse so for longer solves, no need to recompute
## makeCacheMatrix creates a list containing functions to set value of the list, get the value of the list, set the value of the inverse_matrix, get the value of the inverse_matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inverse_matrix <- NULL
	set_matrix <- function(y) {
		x <<- y
		inverse_matrix <<- NULL
	}
	
	get_matrix <- function() x
	set_inverse <- function(solve_matrix) inverse_matrix <<- solve_matrix
	get_inverse <- function() inverse_matrix
	list(set_matrix = set_matrix, get_matrix = get_matrix,
		set_inverse = set_inverse,
		get_inverse = get_inverse)
}


## Write a short comment describing this function
## cacheSolve computes the inverse of the created matrix above
## if the inverse has already been calculated and the matrix is unchanged then
## cacheSolve retrieves the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse_matrix <- x$get_inverse()
	if(!is.null(inverse_matrix)) {
	message("getting cached matrix")
	return(inverse_matrix)
	}
	
	data <- x$get_matrix()
	inverse_matrix <- solve(data, ...)
	x$set_inverse(inverse_matrix)
	return(inverse_matrix)
}

