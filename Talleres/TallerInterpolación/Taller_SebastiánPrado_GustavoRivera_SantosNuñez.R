require(pracma)
require(PolynomF)
require(Matrix)

#----------------------------------------------------
#                      Punto 2
#----------------------------------------------------

x <- c(0,1,2)
y <- c(10,15,5)


f <- cubicspline(x, y, xi = NULL, endp2nd = FALSE, der = c(1,1))
ffun <- function(x) ppval(f,x)

#print(f)

g = function(x) 4.25*x^3-11.75*x^2-2.5*x+15

h = function(x) -4.25*x^3+1*x^2+8.25*x+10

#m = function(x) -8.5*x^3+12.75*x^2+10.75*x+25

#curve(h, xlim=c(-1,2))

plot(x,y, xlim = c(-1,2), col = "red", axes = FALSE, ylab="")
par(new=TRUE)
plot(ffun, xlim=c(-1,2), col = "blue")

#---------------------------------------------------
#                      Punto 4
#---------------------------------------------------

newtonInterpolacion = function(x, y, a) {
  n = length(x)
  A = matrix(rep(NA, times = n^2), nrow = n, ncol = n)
  A[,1] = y
  for (k in 2:n) {
    A[k:n, k] = (A[k:n, k-1] - A[(k-1):(n-1), k-1] ) / (x[k:n] - x[1:(n-k+1)])
  }
  # Imprimir matriz de diferencias divididas
  #print(A)
  # Evaluar
  smds = rep(NA, length = n)
  smds[1] = 1 #x = x[1],..., x[n] pues n = length(x)
  for (k in 2:n) {
    smds[k] = (a - x[k-1])*smds[k-1] # hasta x[n-1]
  }
  return(sum(diag(A)*smds) )
}

f = function(x) log(x)

datx = seq(1,2,by =1/10); daty = c()
x = 1
repeat
{
  daty[x] = f(datx[x])
  x = x + 1
  if(x == length(datx)+1)
    break;
}

i = 1
sum = 0
repeat
{
  val = newtonInterpolacion(datx, daty, datx[i])
  cat("x", datx[i], "f(x)",f(datx[i]), "Pn(x)", val, "error absoluto", abs(f(datx[i])-val), "\n")
  sum = sum + abs(f(datx[i])-val)
  i = i + 1
  if(i == length(datx)+1)
    break;
}

cat("La suma de errores es:", sum/10)

#---------------------------------------------------
#                      Punto 5
#---------------------------------------------------

cat("\nMano\n\n")

#interpolacion
##Puntos 
x=c(14.6, 14.7, 14.6, 14.8, 15.2, 15.6, 15.7, 17.0, 17.6, 17.5, 17.3, 16.8, 15.4, 14.8, 14.4, 14.5, 15.0, 15.1, 15.0, 14.9, 14.6, 14.3, 14.0, 13.9, 13.8, 13.5, 13.1, 13.0, 13.3, 13.2, 13.1, 12.9, 12.4, 11.9, 11.7, 11.6, 11.3, 10.9, 10.7, 10.6, 10.1, 9.7, 9.4, 9.3, 9.6, 9.9, 10.1, 10.2, 10.3,10, 9.5, 9.10 ,8.6, 7.5, 7.0, 6.7, 6.6, 7.70, 8.00, 8.10, 8.40,              9.00, 9.30, 10, 10.2, 10.3)                                                                                                       
y=c(14.7, 14.0, 13.4, 12.3, 11.0, 10.5, 10.2, 8.20, 7.10, 6.70, 6.60, 6.80, 8.30, 8.80, 9.30, 8.80, 6.30, 5.50, 5.00, 4.70, 4.60, 4.50, 4.90, 5.40, 5.80, 6.90, 8.20, 7.60, 5.80, 4.50, 4.30, 3.90, 4.20, 5.70, 7.00, 7.90, 8.20, 7.30, 6.70, 5.10, 4.60, 4.7, 5.0, 5.5, 7.2, 7.8, 8.60, 9.40, 10.0,10.7, 11, 10.7, 9.9, 9.0, 9.1, 9.3, 9.7, 11.7, 12.3, 12.5, 13.0,              13.9, 14.9, 16, 16.4, 16.8)
vectorm = c()
#z = seq(1,65, by = 2)
#x = x[-z]
#y = y[-z]
plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Diagrama")

i = 1
repeat
{
  m = (y[i+1]-y[i])/(x[i+1]-x[i])
  vectorm[i] = m
  i = i + 1
  if (length(x) == i)
    break;
}

i = 1
repeat
{
  if (abs(vectorm[i]-vectorm[i+1]) < 14) 
  {
    borrar = i 
    x = x[-borrar]
    y = y[-borrar]
  }
  i = i + 1
  if(i == length(vectorm))
  {
    break;
  }
}

length(x)
plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Diagrama")

i= 1
min = 1
max = i+1
bool = 0
cont = 1
repeat
{
  m = (y[i+1]-y[i])/(x[i+1]-x[i])
  cont = cont +1
  cat("bool", bool, "m", m,"i", i, "j", i+1, "cont", cont-1,"<>0\n")
  if (i == 1 && m > 0)
  {
    bool = 1
  }
  else if(i == 1 && m < 0)
  {
    bool = 0
  }
  if (m < 0)
  {
    if (bool == 0)
    {
      j = i + 1
      max= j
      i = j
    }
    else 
    {
      datx = x[min:max]; daty = y[min:max]
      polyAjuste = poly_calc(datx, daty)
      cat("bool", bool, "min", min, "max", max, "cont", cont-1,"<0\n")
      curve(polyAjuste,from=x[min],to=x[max],add=T, lwd=1,col="blue")
      min = max
      cont = 1
      bool = 0
      i = max
      j = i + 1
      max= j
    }
  }
  else
  {
    if (bool == 0)
    {
      datx = x[min:max]; daty = y[min:max]
      polyAjuste = poly_calc(datx, daty)
      cat("bool", bool, "min", min, "max", max, "cont", cont-1,">0\n")
      curve(polyAjuste,from=x[min],to=x[max],add=T, lwd=1,col="blue")
      min = max
      cont = 1
      bool = 1
      i = max
      j = i + 1
      max= j
    }
    else
    {
      j = i + 1
      max= j
      i = j
    }
  }
  if (cont == 2)
  {
    datx = x[min:max]; daty = y[min:max]
    polyAjuste = poly_calc(datx, daty)
    cat("bool", bool, "min", min, "max", max, "cont", cont-1,"<>0\n")
    curve(polyAjuste,from=x[min],to=x[max],add=T, lwd=1,col="blue")
    min = max
    cont = 1
    i = max
    j = i + 1
    max= j 
    
    if(m < 0)
    {
      bool = 0 
    }
    else
    {
      bool = 1
    }
  }
  
  if (i==ceiling(length(x)/2)-1)
  {
    datx = x[min:max]; daty = y[min:max]
    polyAjuste = poly_calc(datx, daty)
    cat("x", i, "y", j, "min", min, "max", max, "cont", cont,"<>0\n")
    curve(polyAjuste,from=x[min],to=x[max],add=T, lwd=1,col="blue")
    break;
  }
}

i= ceiling(length(x)/2)
min = i
max = i+1
bool = 0
cont = 1
repeat
{
  m = (y[i+1]-y[i])/(x[i+1]-x[i])
  cont = cont +1
  cat("bool", bool, "m", m,"i", i, "j", i+1, "cont", cont-1,"<>0\n")
  if (i == 1 && m > 0)
  {
    bool = 1
  }
  else if(i == 1 && m < 0)
  {
    bool = 0
  }
  if (m < 0)
  {
    if (bool == 0)
    {
      j = i + 1
      max= j
      i = j
    }
    else 
    {
      datx = x[min:max]; daty = y[min:max]
      polyAjuste = poly_calc(datx, daty)
      cat("bool", bool, "min", min, "max", max, "cont", cont-1,"<0\n")
      curve(polyAjuste,from=x[min],to=x[max],add=T, lwd=1,col="blue")
      min = max
      cont = 1
      bool = 0
      i = max
      j = i + 1
      max= j
    }
  }
  else
  {
    if (bool == 0)
    {
      datx = x[min:max]; daty = y[min:max]
      polyAjuste = poly_calc(datx, daty)
      cat("bool", bool, "min", min, "max", max, "cont", cont-1,">0\n")
      curve(polyAjuste,from=x[min],to=x[max],add=T, lwd=1,col="blue")
      min = max
      cont = 1
      bool = 1
      i = max
      j = i + 1
      max= j
    }
    else
    {
      j = i + 1
      max= j
      i = j
    }
  }
  if (cont == 2)
  {
    datx = x[min:max]; daty = y[min:max]
    polyAjuste = poly_calc(datx, daty)
    cat("bool", bool, "min", min, "max", max, "cont", cont-1,"<>0\n")
    curve(polyAjuste,from=x[min],to=x[max],add=T, lwd=1,col="blue")
    min = max
    cont = 1
    i = max
    j = i + 1
    max= j 
    
    if(m < 0)
    {
      bool = 0 
    }
    else
    {
      bool = 1
    }
  }
  
  if (i==ceiling(length(x)))
  {
    break;
  }
}

#----------------------------------------------------
#                      Punto 8
#----------------------------------------------------

#----
#  a
#----

t = c(100,200,300,400,500,600)
b = c(-160, -35, -4.2, 9, 16.9, 21.3)

#P5 = a0+a1(x-x0)+a2(x-x0)(x-x1)+a3(x-x0)(x-x1)(x-x2)+a4(x-x0)(x-x1)(x-x2)(x-x3)+a5(x-x0)(x-x1)(x-x2)(x-x3)(x-x4)

a = matrix(c(1,t[1], t[1]^2, t[1]^3, t[1]^4, t[1]^5,
             1,t[2], t[2]^2, t[2]^3, t[2]^4, t[2]^5,
             1,t[3], t[3]^2, t[3]^3, t[3]^4, t[3]^5,
             1,t[4], t[4]^2, t[4]^3, t[4]^4, t[4]^5,
             1,t[5], t[5]^2, t[5]^3, t[5]^4, t[5]^5,
             1,t[6], t[6]^2, t[6]^3, t[6]^4, t[6]^5), byrow = T, nrow = 6, ncol = 6) # 90 multiplicaciones

y = solve(a, b)

p5 = function(x) y[1]+y[2]*x+y[3]*x^2+y[4]*x^3+y[5]*x^4+y[6]*x^5

cat("\nPolinomio: ",y[1],"+",y[2],"*x+",y[3],"*x^2+", y[4],"*x^3+",y[5],"*x^4+",y[6],"*x^5\n\n")

#-----
#  b
#-----

B = p5(450)

cat("\nEl segundo coeficiente viral con T = 450 es:", B, "\n\n")

#Con V = 227000 cm^3, R = 8,314472 (cm^3*Mpa)/(K*mol) y P = 10^5 pascales -> 1e-5 Mpa

V = 227000
P = 1e-5
R = 8.314472

PV = P*V
RT = R*450

cat("\nEl resultado de PV/RT con T = 450 es:",PV/RT, "\n\n")

C = ((PV/RT)-1-(B/V))*(V^2)

cat("\nEl tercer coeficiente viral con T = 450, V = 227000 cm^3, R = 8,314472 (cm^3*Mpa)/(K*mol) y P = 10^5 pascales -> 1e-5 Mpa es aproximadamente:",C, "\n\n")

cat("\nEl resultado de la operación 1+B/V+C/V^2 es",1+B/V+C/V^2, "que es igual a PV/RT")

#-----
#  c
#-----

plot(p5, xlim = c(50,650), col="white",main = "Por Sistema de Ecuaciones", axes=FALSE, xlab="t", ylab = "b")
par(new=TRUE)
plot(t,b, xlim=c(50, 650), col="red")
curve(p5, add=T)

#-----
#  d
#-----

plot(p5, xlim = c(50,650), col="white",main = "Por Sistema de Ecuaciones", axes=FALSE, xlab="t", ylab = "b")
par(new=TRUE)
plot(t,b, xlim=c(50, 650), col="red")
curve(p5, add=T)

g = poly_calc(t,b)
curve(g, col = "green", add = T)

cat("\nEl Polinomio con poly_calc es: ")
print(g)
cat("\n\n")

cat("\nB en T = 450 es:",g(450),"\n\n")

f = lagrangeInterp(t,b, 450)

cat("\n pero el valor con la funcion LagrangeInterp en T=450 es:", f,"\n\n")

#-----
#  e
#-----

cat("\nTeniendo en cuenta ambos resultados, el teorico es mejor debido a que el resultado es logico (pues da entre el
intervalo de [400,500]) y la grafica pasa por todos los puntos ademas el de poly_calc da un valor muy raro y la
grafica solo pasa por un punto. Utilizando otro metodo de lagrange en Pracma el resultado es bastante similar al
teorico. Por otro lado la cantidad de multiplicaciones en el teorico fueron 90 y en lagrange al ser un polinomio
sacado de 6 puntos se requieren 6 ciclos y por cada ciclo es necesario hacer 2n multiplicaciones (12) por lo que
se realizan 72 multiplicaciones. Concluyendo, el metodo de lagrange es mas eficiente.")