#Programa 3

main <- function(){
  cat("1. Graficar la función de densidad de una distribución uniforme. \n\n")
  cat("2. Graficar la función de densidad de una distribución Bernoulli. \n\n")
  cat("3. Graficar la función de densidad de una distribución Poisson. \n\n")
  cat("4. Graficar la función de densidad de una distribución Exponencial. \n\n")
  val <- as.integer(readline("Ingresar opción: "))
  if (val==1){
  
    cat("Para graficar la función de densidad de una distribución uniforme: \n\n")
    
    
    maxl <- as.numeric(readline("Ingrese el limite superior que desea: "))
    minl <- as.numeric(readline("Ingrese el inferior que desea: "))
    
    #verificar que el limite inferior sea menor que el limite superior   
    if(maxl < minl) {
      stop("El limite inferior debe ser menor que el limite superior")
    }
    
    
    #Grafica de distribución uniforme
    curve(dunif(x,minl,maxl), from=minl-0.5, to=maxl+0.5)
    
    
    
  }else if(val==2){
    
    cat("Para graficar la función de densidad de una distribución Bernoulli: \n\n")
    
    xl <- as.numeric(readline("Ingrese el limite de valores del eje x: "))
    
    #vector correspondiente al eje x
    x <- 1:xl
    
    n <- as.numeric(readline("Ingrese el número de ensayos: "))
    
    #verificar que el número de ensayos no sea mayor que el eje x
    if(n > length(x)){
      stop("El número de ensayos no puede ser mayor al eje x")
    }
    
    p <- as.double(readline("Ingrese la probabilidad de éxito en cada ensayo: "))
    
    #verificar que la probabilidad sea mayor o igual a 1
    if(p >= 1){
      stop("La probabilidad no puede ser mayor a 1")
    }
    
    #Grafica distribución de Bernoulli o Binomial
    plot(dbinom(x, size = n, prob = p), type = "h", lwd = 2,
         main = "Función de probabilidad de Bernoulli o Binomial",  
         ylab = "P(X = x)", xlab = "Número de éxitos")
    
  
    
  }else if(val==3){
    
    cat("Para graficar la función de densidad de una distribución de Poisson: \n\n")
    
    xl <- as.numeric(readline("Ingrese el limite de valores del eje x: "))
    
    #vector correspondiente al eje x
    x <- 0:xl
    
    lda <- as.numeric(readline("Ingrese la media de eventos que ocurren en el intervalo (lambda): "))
    
    #Graficar distribución
    plot(dpois(x, lda), type = "h", lwd = 2,
         main = "Función de masa de probabilidad distribución de Poisson",
         ylab = "P(X = x)", xlab = "Número de eventos")
    
    # Leyenda
    legend("topright", legend = c(lda),
           title = expression(lambda), title.adj = 0.85,
           lty = 1, col = 1, lwd = 2, box.lty = 0)
    
  }else if(val==4){
    
    cat("Para graficar la función de densidad de una distribución Exponencial: \n\n ")
    
    xl <- as.numeric(readline("Ingrese el limite de valores del eje x: "))
    
    #vector correspondiente al eje x
    x <- seq(0, xl, 0.1)
    
    lda <- as.numeric(readline("Ingrese el valor de lambda (Recuerde que el valor de lambda debe ser mayor que 0): "))
    
    if(lda <= 0){
      stop("El valor de lambda debe ser mayor a 0")
    }
    
    #Graficar distribución exponencial
    plot(x, dexp(x, lda), type = "l",
         ylab = "f(x)", lwd = 2, col = "red")
    
    # Leyenda
    legend("topright", legend = c(lda),
           title = expression(lambda), title.adj = 0.85,
           lty = 1, col = c("red"), lwd = 2, box.lty = 0)
    
    
  }else{
    print("Ingrese una opción valida")
  }
}

