# Cuando se manejan gran cantidad de datos, se realizan calculos que toman mucho tiempo.
# Por esto, es importate almacenar esos calculos, que luego, pueden ser requeridos nuevamente 


# Función makeCacheMatrix
#  
# Almacena una matriz y almacena en caché su inversa

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y){
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inversa <<- solveMatrix
  getInverse <- function() inversa
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Esta función calcula la inversa de la "matriz" especial creada por
# makeCacheMatrix arriba. Si la inversa ya se ha calculado (y la matriz no ha cambiado), 
# entonces debería recuperar la inversa de la caché

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Obteniendo datos de cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv 
}

# Probando las funciones 

# Generando la matriz. 
# Tener en cuenta que la matriz mi_matriz sea invertible de lo contrario no funcionara correctamente 

matriz <- matrix(sample(1:20, 20, replace = TRUE), nrow = 4, ncol = 4)
matriz

# Evaluar matriz en makeCacheMatrix, almacenarlo en mi_matriz
mi_matriz <- makeCacheMatrix(matriz)
mi_matriz $ get()
mi_matriz $ getInverse()

# Evaluar mi
cacheSolve(mi_matriz)
mi_matriz$getInverse()
