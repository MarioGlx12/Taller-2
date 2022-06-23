#PROGRAMA 2

#Ejercicio1

#Generando Archivos csv
vecA= rnorm(100)
write.csv(vecA, "Experimento_a.csv",
          row.names = FALSE)

vecB= rnorm(100)
write.csv(vecB, "Experimento_b.csv",
          row.names = FALSE)

#Leer Archivos csv
Experimento_a<- read.csv("Experimento_a.csv")
Experimento_b<- read.csv("Experimento_b.csv")

#Crea un dataframe con los dos experimento
df<- data.frame(Experimento_a$x,
                Experimento_b$x)

val=0



#Funcion
main <- function(){
  val<-0
  while(val != 5){
    
    print("1. Aplicar t-student ")
    print("2. Correlacion de Pearson ")
    print("3. Correlacion de Spearman ")
    print("4. Graficar diagrama de dispersion ")
    print("5. Salir")
    
    val <- as.integer(readline("Ingresar opcion: "))
    
    if(val==1){
      #Aplica el test student a los dos experimento
      test=t.test(df$Experimento_a,df$Experimento_b)
      
    if(test$p.value>0.05){
      print("La diferencia en la media de los datos no es estadisticamente significativa.")
    
      }else{
        print("La diferencia en la media de los datos es estadisticamente significativa.")
      }
    }else if (val==2){
      
      #Ejercicio2
      pearson=cor(df$Experimento_a, df$Experimento_b, method = c("pearson"))
      cat(sprintf("La correlacion de Pearson es: %f \n" ,pearson))
      
    }else if(val==3){
      spearman=cor(df$Experimento_a, df$Experimento_b, method = c("spearman"))
      cat(sprintf("La correlacion de Spearman es: %f \n" ,spearman))
      
    }else if(val==4){
      
      #Ejercicio3
      plot(x = df$Experimento_a, y = df$Experimento_b)  
      regresion <- lm(df$Experimento_a ~ df$Experimento_b)
      abline(regresion)
    }
  }
}
