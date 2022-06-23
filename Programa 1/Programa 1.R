#Programa 1

#Descomentar estas lineas borrando el # y despues ejecutarlas para poder instalar las librerias correspondientes

#install.packages("gapminder") 
#install.packages("ggplot2")
#install.packages(dplyr)
#install.packages("openxlsx")

library(gapminder)
library(ggplot2)
library(dplyr)
library(openxlsx)

gp_mnd = gapminder




main <- function(){
  cat("1. Guardar gapminder \n\n")
  cat("2. Leer gapminder.xlsx \n\n")
  cat("3. Graficar el diagrama de dispersión lifeEx vs pop. \n\n")
  cat("4. Graficar el diagrama de dispersión gdpPercap vs pop. \n\n")
  cat("5. Graficar los diagramas de cajas de la variable gdpPercap \n") 
  cat("discriminados por continentes desde 1990 a 2007. \n\n")
  val <- as.integer(readline("Ingresar opción: "))
  if (val==1){
    
    print("Voy a guardar gapminder")
    
    #hallar el 10% de las columnas lifeExp, pop y gdpPercap
    le_10 = trunc (length(gp_mnd$lifeExp)*0.10)
    pop_10 = trunc (length(gp_mnd$pop)*0.10)
    gdp_10 = trunc (length(gp_mnd$gdpPercap)*0.10)
    
    
    #Seleccionar aleatoriamente el 10% de las columnas lifeExp, pop y gdpPercap
    
    ind_le = sample(1:length(gp_mnd$lifeExp), le_10)
    ind_pop = sample(1:length(gp_mnd$pop), pop_10)
    ind_gdp = sample(1:length(gp_mnd$gdpPercap), gdp_10)
    
    #Asignar NA (valores no asignados) al 10% de las columnas
    
    gp_mnd$lifeExp[ind_le] = NA
    gp_mnd$pop[ind_pop] = NA
    gp_mnd$gdpPercap[ind_gdp] = NA
    
    #Exportar a xlsx
    write.xlsx(gp_mnd, "gapminder.xlsx")
    
  }else if(val==2){
    print("Voy a leer gapminder")
    
    #Importar xlsx en un dataframe
    GapMinder <- read.xlsx(xlsxFile="gapminder.xlsx")
    
    #Imprimir el dataframe al que se importo el xlsx
    print(GapMinder)
    
    
    
  }else if(val==3){
    
    
    print("Voy a graficar el diagrama de dispersión lifeExp vs pop.")
    
    #Importar xlsx en un dataframe
    GapMinder <- read.xlsx(xlsxFile="gapminder.xlsx")
    
    #Selecciona las columnas del dataframe y las gráfica en un diagrama de dispersión
    GapMinder %>% 
      select(pop,lifeExp) %>% 
      ggplot(aes(x=log(pop),y=lifeExp))+geom_point()
    
    
  }else if(val==4){
    
    print("Voy a graficar el diagrama de dispersión gdpPercap vs pop.")
    
    #Importar xlsx en un dataframe
    GapMinder <- read.xlsx(xlsxFile="gapminder.xlsx")
    
    #Selecciona las columnas del dataframe y las gráfica en un diagrama de dispersión
    GapMinder %>% 
      select(pop,gdpPercap) %>% 
      ggplot(aes(x=log(pop),y=log(gdpPercap)))+geom_point()
    
  }else if(val==5){
    
    print("voy a graficar los diagramas de cajas de la variable gdpPercap discrimina-
dos por continentes desde 1990 a 2007.")
    
    #Importar xlsx en un dataframe
    GapMinder <- read.xlsx(xlsxFile="gapminder.xlsx")
    
    #Reemplazar NA por el valor minimo en la columna gdpPercap
    GapMinder["gdpPercap"] <- lapply(GapMinder["gdpPercap"], function(x) replace(x,is.na(x), min(x, na.rm = T) ))
    
    df=GapMinder %>% 
      select(year,continent,gdpPercap) %>% 
      filter(year >= 1990 & year <=2007)
    
    boxplot(df$gdpPercap ~ df$continent, xlab = "Continent", ylab = "gdpPercap")
    
    
    
    
  }else{
    print("No haz seleccionado una opción valida")
  }
}


