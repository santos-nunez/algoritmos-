#-----------------------------------------------------------------------------------------------------------
#                                                 Taller Derivadas
#-----------------------------------------------------------------------------------------------------------

#----------------
#     A y B
#----------------

f = function(x)
{
  x*cos(x)
}

h = c(0.1, 0.01, 0.001, 0.0001)

derivada = function(i, x0, h)
{
  n = length(h)
  m = 1
  
  original = D(parse(text="x*cos(x)"), "x")
  fp = function(x) eval(original)
  fp(x0)
  
  cat("h \t D \t Error \n")
  repeat
  {
    ip= (i(x0+h[m])-i(x0))/(h[m])
    E = abs(ip-fp(1.8))
    cat(h[m], "\t", ip, "\t", E, "\n")
    m=m+1
    if(m>n)
    {
      break;
    }
  }
}

derivada(f, 1.8, h)

#----------------
#       c
#----------------

derivada2 = function(i, x0, h)
{
  n = length(h)
  m = 1
  
  original = D(parse(text="x*cos(x)"), "x")
  fp = function(x) eval(original)
  fp(x0)
  ho = 0
  val = 0 ; 
  repeat
  {
    ip= (i(x0+h[m])-i(x0))/(h[m])
    E = abs(ip-fp(1.8))
    m=m+1
    val = E*10^3
    if(val >= 1 && val < 10)
    {
      ho = h[m]
    }
    if(m>n)
    {
      break;
    }
    
  }
  cat("\n El valor del h que proporciona una precisió de 10^-4 es: ", ho)
  
}
derivada2(f, 1.8, h)


#-------------------------
#         D
#-------------------------


derivacion3puntos = function(x0,h,f)
{
  n = length(h)
  m = 1
  foriginal = D(parse(text="x*cos(x)"), "x")
  fp = function(x) eval(foriginal)
  fp(x0)
  repeat
  {
    g = (4*f(x0+h[m])-3*f(x0)-f(x0+2*h[m]))/(2*h[m])
    error= abs(g-fp(1.8))                           #Es el error de truncamiento
    cat(h[m], "\t", g, "\t", error, "\n")
    m=m+1
    if(m>n)
    {
      break
    }
  }
}

derivacion3puntos(1.8,h,f)

#----------------------------------
#           E
#---------------------------------

derivacion3ModPuntos = function(x0,h,f)
{
  n = length(h)
  m = 1
  foriginal = D(parse(text="x*cos(x)"), "x")
  fp = function(x) eval(foriginal)
  fp(x0)
  repeat
  {
    g = (4*f(x0)-3*f(x0-h[m])-f(x0+h[m]))/(2*h[m])
    
    error= abs(g-fp(1.8))                           #Es el error de truncamiento
    cat(h[m], "\t", g, "\t", error, "\n")
    m=m+1
    if(m>n)
    {
      break
    }
  }
}

derivacion3ModPuntos(1.8,h,f)

#----------------------------------
#           F
#----------------------------------

derivacionf = function(x0,h,f)
{
  n = length(h)
  m = 1
  foriginal = D(parse(text="cos(x)-x*sin(x)"), "x")
  fp = function(x) eval(foriginal)
  fp(x0)
  repeat
  {
    g = (4*f(x0+2*h[m])-5*f(x0+h[m])+2*f(x0)-f(x0+3*h[m]))/(h[m]^2)
    
    error= abs(g-fp(1.8))                           #Es el error de truncamiento
    cat(h[m], "\t", g, "\t", error, "\n")
    m=m+1
    if(m>n)
    {
      break
    }
  }
}

derivacionf(1.8,h,f)
#----------------------------------
#           G
#----------------------------------
f = function(x) x*cos(x)
foriginal = D(parse(text="x*cos(x)"), "x")
fp = function(x) eval(foriginal)
xo = 1.8
h = 0.1
original = fp(xo)

der = function(f, xo, h,original)
{
  i = 0
  cat("\nEl valor 'exacto' de la derivada es ", original,"\n")
  while(i < 5)
  {
    d = (f(xo-2*h)-8*f(xo-h)+8*f(xo+h)-f(xo+2*h))/(12*h)
    h = h /10
    i = i + 1
    e = abs(original - d)
    
    cat("La derivada es ", d, " El error de truncamiento es ", e, "\n")
  }
}
der (f, xo, h,original)

#----------------------------------
#           I
#----------------------------------
f = function(x) x*exp(x)
r = 5.436563656918091
h = 0.1
i = 0
x = c()
y = c()

while(i < 16)
{
  d =(f(1+h)-f(1))/h
  error = abs(r-d)
  h = h / 10
  i = i + 1
  x[i] = h
  y[i] = error
}

plot(x,y)#poner lineas 
lines(x,y)

#----------------------------------
#           J
#----------------------------------

t = c(1.00,1.01,1.02,1.03,1.04)
iCorriente = c(3.10,3.12,3.14,3.18,3.24)
h = 0.01
xo = 1
R = 0.142
L = 0.98

encontrarValor = function(t, buscado)
{
  i = 1
  tama = length(t)
  while(i < tama +1)
  {
    if(t[i] == buscado)
    {
      return (i)
    }
    i = i + 1
  }
}


f = function(iCorriente, t, h, x0,R, L)
{
  i = 1
  k = 0.01
  tama = length(iCorriente)
  while(i < tama)
  {
    valor1 = encontrarValor(t, xo+h)
    valor2 = encontrarValor(t, xo+h-k)
    d = (iCorriente[valor1]-iCorriente[valor2])/(k)
    h = h + 0.01
    i = i + 1
    cat("La derivada es ", d, " el voltaje en ", t[i-1], " es", L*d+R*iCorriente[i-1],"\n")
  }
}
f(iCorriente, t, h, xo, R, L)

