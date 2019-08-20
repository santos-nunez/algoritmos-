#------------------------------------------------------------------------------------------------------
#                                           Taller Matrices 2
#------------------------------------------------------------------------------------------------------

library(pracma)
library(Matrix)

condic<-function (A)
{
  IA = solve(A)
  return(norm(A)*norm(IA))
}

aleatorio<-function()
{
  A = c(0)
  A = matrix(A, 6, 6)
  c <- 0
  detD=0
  while(c<=1000 || detD==0)
  {
    i<-1
    repeat
    {
      j<-1
      repeat
      {
        A[i,j] = sample(0:20, 1)
        j<-j+1
        if(j>6)
          break
      }
      i<-i+1
      if(i>6)
        break
    }
    
    c <- condic(A)
    D<-diag(diag(A))
    detD=det(D)
    
  } 
  return(A)
}

radioEspectralJacobi<-function(A,B)
{
  D<-diag(diag(A))
  L<-tril(A, k=-1)
  U<-triu(A, k=1)
  
  ID<- solve(D)
  
  TJac<- (-1*ID)%*%(L+U)
  
  cat("\nMatriz A:\n\n")
  print(A)
  cat("\nDiagonal:\n\n")
  print(D)
  cat("\nTriangular Inferior:\n\n")
  print(L)
  cat("\nTriangular Superior:\n\n")
  print(U)
  cat("\nTransicion en Jacobi:\n\n")
  print(TJac)
  
  nT = norm(TJac, type = c("I"))
  cat("\n\tRadio Espectral: ", nT);
  
  cat("\n\nVector B para la solucion:\n\n")
  print(B)
  
  x0 = rep(c(1), sqrt(length(A)))
  
  X = itersolve(A, B, x0, tol = 1e-8, method = "Jacobi")
  cat("\nSolución por Jacobi:\n\n")
  print(X$x)
  
}

radioEspectralGauss<-function(A,B)
{
  L<-tril(A)
  IL<-solve(L)
  
  TGauss<- (eye(sqrt(length(A)))- (-IL%*%A))
  
  cat("\nMatriz A:\n\n")
  print(A)
  cat("\nTriangular Inferior:\n\n")
  print(L)
  cat("\nTransicion en Gauss Seidel:\n\n")
  print(TGauss)
  
  nT = norm(TGauss, type = c("I"))
  cat("\n\tRadio Espectral: ", nT);
  
  cat("\n\nVector B para la solucion:\n\n")
  print(B)
  
  x0 = rep(c(1), sqrt(length(A)))
  
  X = itersolve(A, B, x0, tol = 1e-8, method = "Gauss-Seidel")
  cat("\nSolución por Gauss Seidel:\n\n")
  print(X$x)
  
}

radioEspectralSOR<-function(A,B, w)
{
  D<-diag(diag(A))
  L <- tril(A, k=-1)
  Qw <- D/w + L
  IQw <- solve(Qw)
  
  TSOR <- eye(sqrt(length(A))) - IQw%*%A
  
  cat("\nMatriz A:\n\n")
  print(A)
  cat("\nDiagonal:\n\n")
  print(D)
  cat("\nTriangular Inferior:\n\n")
  print(L)
  cat("\nTransicion en SOR:\n\n")
  print(TSOR)
  
  nT = norm(TSOR, type = c("I"))
  cat("\n\tRadio Espectral: ", nT);
  
  cat("\n\nVector B para la solucion:\n\n")
  print(B)
  
  x0 = rep(c(1), sqrt(length(A)))
  
  X = itersolve(A, B, x0, tol = 1e-8, method = "Richardson")
  cat("\nSolución por SOR:\n\n")
  print(X$x)
}

#----------------------------------------
#             Primera Matriz
#----------------------------------------

A=aleatorio()
B=c(1,2,3,4,5,6)

radioEspectralJacobi(A, B)
radioEspectralGauss(A, B)
radioEspectralSOR(A, B, 2)

#----------------------------------------
#             Segunda Matriz
#----------------------------------------

A= matrix(c(8,2,2,9,7,8,2,2,6), 3, 3)
B=c(69,47,68)

radioEspectralJacobi(A, B)
radioEspectralGauss(A, B)
radioEspectralSOR(A, B, 2)
