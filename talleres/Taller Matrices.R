#----------------------------------------------------------------------------------------------------------
#                                             Taller Matrices
#----------------------------------------------------------------------------------------------------------

library(pracma)
library(Matrix)

A = matrix(c(-8.1, -7, 6.123, -2, -1, 4,
             -3, -1, 0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)

B = c(1.45,3,5.12,-4)

#-------------------------------------------------------------
#                         Primer Punto
#-------------------------------------------------------------

#---
# a
#---

n<-4
D1<-eye(n, m = n)
D2<-ones(n, m = n)
D3<-zeros(n, m = n)

cat("Funcion eye con m=n=4 \n\n")
print(D1)
cat("\nFuncion ones con m=n=4 \n\n")
print(D2)
cat("\nFuncion zeros con m=n=4 \n\n")
print(D3)

#---
# b
#---

w = 2

D<-diag(diag(A))
L <- tril(A, k=-1)
Qw <- D/w + L
IQw <- solve(Qw)

Transc <- eye(4) - IQw%*%A

cat("\nMatriz A \n\n")
print(A)
cat("\nMatriz de Transicion de SOR \n\n")
print(Transc)


#-------------------------------------------------------------
#                         Segundo Punto
#-------------------------------------------------------------

#---
# a
#---

D<-diag(diag(A))

L <- tril(A, k=-1)

U<- triu(A, k=1)

A1 = L+D+U

cat("Matriz inicial:\n")
print(A)
cat("Matriz Triangular Inferior:\n")
print(L)
cat("Matriz Diagonal:\n")
print(D)
cat("Matriz Triangular Superior:\n")
print(U)
cat("Matriz Resultante de L+D+U:\n")
print(A1)

#---
# b
#---

cat("Gauss-Seidel:\n\n")
sol = itersolve(A, B, tol=1e-9 , method = "Gauss-Seidel")
print(sol)

#---
# c
#---

jacobi <- function(A,b, x0, iter)
{
  x_k = matrix(x0)
  
  it = 0
  repeat
  {
    inn = matrix(b-((L+U)%*%x_k))
    D1 = (solve(D))
    xk1 = D1%*%inn
    cat("Error en la iteracion",it,"es: ", norm(xk1-x_k,"F")/norm(x_k),"\n")
    x_k = xk1
    it = it + 1
    if(it == iter)
      break
  }
  cat("Solucion a", it ,"iteraciones: ",x_k,"\n")
}


x0 = c(1,2,1,1)

cat("\nMatriz A: \n\n")
print(A)
cat("\nVector B: \n\n")
print(B)
cat("\nx0: \n\n")
print(x0)
cat("\nSolucion por Jacobi con 5 iteraciones: \n\n")
jacobi(A, B, x0, 5)

#-------------------------------------------------------------
#                          Tercer Punto
#-------------------------------------------------------------

#---
# a
#---

PoliCaract <- function(a, info = FALSE)
{
  stopifnot(is.numeric(a), is.matrix(a))
  n <- nrow(a); m <- ncol(a)
  if (n != m || n < 2)
    stop("La matriz debe ser cuadrada")
  if (n > 100)
    cat("El algoritmo va a ser muy lento para n > 100.\n")

  p <- rep(1, n+1)

  rep

  a1 <- a
  for (k in 2:n) {
    p[k] <- -1 * sum(diag(a1))/(k-1)
    if (k == n) a2 <- a1
    a1 <- a %*% (a1 + p[k] * diag(1, n))
  }
  p[n+1] <- -1 * sum(diag(a1))/n

  if (info)
  {
    adet <- (-1)^n * p[n+1]
    if (adet != 0)
      ainv <- -1 * (a2 + p[n] * diag(1, n))/p[n+1]
    else
      ainv = NaN

    # determinar la exactitud
    e <- a2 %*% a + p[n] *a - adet * diag(1, n)
    e <- max(abs(e))
    cat("Error term:", e, "\n")
  }

  if (info)
    return(list(cp = p, det = adet, inv = ainv))
  else
    return(p)
}

poli = PoliCaract(A, info = FALSE)

cat("\nMatriz A: \n\n")
print(A)
cat("\nSolucion por el polinomio caracteristico: \n\n")
print(poli)

#---
# b
#---

L = tril(A,k=-1)
U = triu(A,k=1)
L[lower.tri(L,diag=TRUE)] <- 0
U[upper.tri(U, diag = TRUE)] <- 0

cat("\nMatriz A: \n\n")
print (A)

D = diag(diag(A))
I=diag(1,nrow = nrow(A)) # Matriz diagonal de dimension 3
D1 <- solve(D,I) # Matriz inversa de A
T1 = D1 %*% U
T2 = (I + (L %*% D1))
T2<- solve(T2,I) # Matriz inversa de A
MatTG = T1+T2
normaG = norm(MatTG, type = c( "I"))
cat("\nConvergencia Gauss:\n\n")
print(normaG)
MatTJ = (-D1)%*%(L+U)
normaJ = norm(MatTJ, type = c("I"))
cat("\nConvergencia Jacobi: \n\n")
print(normaJ)

w = 0.00000001

D<-diag(diag(A))
L <- tril(A, k=-1)
Qw <- D/w + L
IQw <- solve(Qw)

MatTSOR <- eye(4) - IQw%*%A
normaSOR = norm(MatTSOR, type = c("I"))
cat("\nConvergencia SOR (con valor w =",w,"): \n\n")
print(normaSOR)

#---
# c
#---

print("\nMatriz transicion Gauss\n\n")
print(MatTG)
print("\nMatriz transicion Jacobi\n\n")
print (MatTJ)
cat("\nMatriz de Transicion de SOR \n\n")
print(MatTSOR)

#---
# d
#---

A = matrix(c(4, -1, -1, -1, -1, 4,
             -1, -1, -1, -1, 4, -1,
             -1, -1, -1, 4), nrow=4, byrow=TRUE)
b = c(1, 5, 1.5,-2.33)

cat("\nMatriz A: \n\n")
print(A)
cat("\nVector B: \n\n")
print(B)

X <- itersolve(A, b, method = "Jacobi")
cat("\nMetodo Jacobi: \n\n")
print(X)
X <- itersolve(A, b, tol = 1e-9 , method = "Gauss-Seidel")
cat("\nMetodo Gauss-Seidel: \n\n")
print(X)
solucion<- solve(A,b)
cat("\nSolucion por Defecto: \n\n")
print(solucion)

#-------------------------------------------------------------
#                          Tercer (2) Punto
#-------------------------------------------------------------

#---
# a
#---

tril1 <- function(M, k = 0) {
  if (k == 0) {
    M[upper.tri(M, diag = TRUE)] <- 0
  } else {
    M[col(M) >= row(M) + k + 1] <- 0
    M[col(M)==row(M)] <- 0
    
  }
  return(M)
}

M = matrix(c(2,3,4,1,2,3,5,6,7),nrow=3)
cat("\nMatriz A3: \n\n")
print(M)
cat("\nMatriz Triangular Inferior: \n\n")
print(tril1(M, k=1))

#---
# b
#---

diag1 <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

M = matrix(c(2,3,4,1,2,3,5,6,7),nrow=3)
cat("\nMatriz A3: \n\n")
print(M)
cat("\nMatriz Diagonal: \n\n")
print(diag1(M))

#-------------------------------------------------------------
#                          Cuarto Punto
#-------------------------------------------------------------

numMult = function(A, b)
{ # Se supone det(A) != 0
  mult = 0
  n = nrow(A) # = ncol(A) para que sea cuadrada
  
  # matriz ampliada
  Ab = cbind(A,b)
  print(Ab)
  # Eliminación
  for (k in 1:(n-1)){ # desde columna k=1 hasta k=n-1
    if(Ab[k,k]==0){ # intercambio de fila
      fila = which(Ab[k, ]!=0)[1]
      Ab[c(k, fila), ] = Ab[c(fila, k), ]
    }
    
    # Eliminación columna k
    for (i in (k+1):n){# debajo de la diagonal
      # Fi = Fi - a_ik/a_kk * Fk, i=k+1,...,n
      Ab[i, ] = Ab[i, ] - Ab[i, k]/Ab[k,k]*Ab[k, ]
      mult = mult + 2*(ncol(Ab))
    }
  }
  
  # Sustitución hacia atrás-------------------------
  # b(i) = A[i, n+1]
  x = rep(NA, times=n)
  x[n] = Ab[n, n+1]/Ab[n,n] # xn = bn/a_nn
  mult = mult + n+1
  
  for(i in (n-1):1 ){
    x[i]= (Ab[i, n+1] - sum(Ab[i, (i+1):n]*x[(i+1):n]) ) /Ab[i,i]
    mult = mult + 2*(n-2)
  }
  #cat(x, "\n")
  cat("Numero de multiplicaciones:", mult, "\n")
  return(x)
}

A = matrix(c( 0, 2, 3, 3, 3,
              -5, -4, 1, 4, 5,
              0, 0, 0, 3, 7,
              -4, -7, -8, 9,7,
              3, 4, 5, 5, 6), nrow=5, byrow=TRUE)
b = matrix(c(1,0,0,0,1), nrow=5, byrow=TRUE)

cat("\nMatriz A4: \n\n")
print(A)
cat("\nVector B4:\n\n")
print(B)
cat("\nNumero de multiplicaciones: \n\n")
print(numMult(A,b))

#-------------------------------------------------------------
#                          Quinto Punto
#-------------------------------------------------------------

#---
# a
#---

beta = 0
alpha = 3

A = matrix(c(2, 0, 1,
             beta,2 , -1,
             -1, 1, alpha), nrow=3, byrow=TRUE)
B = matrix (c(1,2,1),nrow=3, byrow=TRUE)

cat("\nMatriz A5: \n\n")
print(A)
cat("\nVector B: \n\n")
print(B)

L = tril(A,k=-1)
U = triu(A,k=1)
L[lower.tri(L,diag=TRUE)] <- 0
U[upper.tri(U, diag = TRUE)] <- 0
D = diag(diag(A))
I=diag(1,nrow = nrow(A)) # Matriz diagonal de dimension 3
D1 <- solve(D,I) # Matriz inversa de A
MatTJ = (-D1)%*%(L+U)
normaJ = norm(MatTJ, type = c("I"))
cat("\nConvergencia Jacobi: \n\n")
print(normaJ)
cat("\nSolucion Jacobi: \n\n")
print(itersolve(A,B,method = "Jacobi"))

#---
# b
#---

library("plot3D")

x = 0
y = 0
z = 0

diag1 <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

jacobiPr2 <- function(A,b, x0, tol){
  x_k = matrix(x0)
  
  D = diag1(A)
  L = tril(A,k=-1)
  U = triu(A,k=1)
  
  it = 1
  repeat
  {
    inn = matrix(b-((L+U)%*%x_k))
    D1 = (solve(D))
    xk1 = D1%*%inn
    cat("Error ",it," ",norm(xk1-x_k,"F")/norm(x_k),"\n")
    x_k = xk1
    
    x[[it]] = x_k[1]
    y[[it]] = x_k[2]
    z[[it]] = x_k[3]
    cat("Solucion iteracion ",it,": ",x[[it]]," ",y[[it]]," ",z[[it]],"\n")
    it = it + 1
    
    if(it == tol)
      break
  }
  lines3D(x, y, z, colvar = z, col = NULL, add = FALSE, theta = 20, phi = 20)
  cat("Solucion a ", tol ," iteraciones: ",x_k,"\n")
}

x1 = c(1,2,3)
jacobiPr2(A, B, x1, 10)

#-------------------------------------------------------------
#                          Sexto Punto
#-------------------------------------------------------------

A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
cat("\nMatriz A:\n\n")
print(A)
b = matrix(c(1.45,3,5.12,-4), nrow=4, byrow=TRUE)
cat("\nVector B:\n\n")
print(b)

#matrices Triangulares
L = tril(A,k=-1)
U = triu(A,k=1)
cat("\nMatriz Triangular Inferior:\n\n")
print(L) 
cat("\nMatriz Triangular Superior:\n\n")
print(U)

#factorizacion QR
gs <- gramSchmidt(A)
Q <- gs$Q 
R <- gs$R
cat("\nMatriz Q:\n\n")
print(Q)
cat("\nMatriz R:\n\n")
print(R)
cat("\nMatriz Resultante (Q*R) :\n\n")
print(Q %*% R)

#-------------------------------------------------------------
#                          Septimo Punto
#-------------------------------------------------------------

#---
# a
#---

library(BB)
ecuaciones = function(x) {
  n = length(x)
  F = rep(NA, n)
  F[1] = x[1] - x[2]
  F[2] = x[1]^2 + x[2]^2 -1
  F
}
p0 = c(1,1) # n initial starting guess
sol = BBsolve(par=p0, fn=ecuaciones)
sol$par

plot(sol$par)
plot(ecuaciones)

#---
# b
#---

trigexp = function(x) {
  
  #Tamaño del vector que llega por parámetro
  n = length(x)
  
  #se crea un vector F vacío
  F = rep(NA, n)
  
  #Se enuncian las ecuaciones del sistema
  F[1] = 3*x[1]^2 + 2*x[2] - 5 + sin(x[1] - x[2]) * sin(x[1] + x[2])
  #Se crea una secuencia de 2 hasta n-1
  tn1 = 2:(n-1)
  #Se evalúan tn1 ecuaciones
  F[tn1] = -x[tn1-1] * exp(x[tn1-1] - x[tn1]) + x[tn1] *
    ( 4 + 3*x[tn1]^2) + 2 * x[tn1 + 1] + sin(x[tn1] -
                                               x[tn1 + 1]) * sin(x[tn1] + x[tn1 + 1]) - 8
  #Se evalúa la última ecuación n
  F[n] = -x[n-1] * exp(x[n-1] - x[n]) + 4*x[n] - 3
  #Se retorna F
  F
}
n = 10000
p0 = runif(n) # n initial random starting guesses
#se halla la solcuión del sistema trigexp usando BBsolve de la librería BB, utilizando n valores iniciales
sol = BBsolve(par=p0, fn=trigexp)
#Muestra el vector solución del sistema para cada n valores iniciales
sol$par

#-------------------------------------------------------------
#                          Octavo Punto
#-------------------------------------------------------------

N <- 3
A <- Diag(rep(3,N)) + Diag(rep(-2, N-1), k=-1) + Diag(rep(-1, N-1), k=1)
x0 <- rep(0, N)
b = c(4,5,6)

itersolve(A, b, tol=1e-9 , method = "Gauss-Seidel")

