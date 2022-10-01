## Mayra Catalina Barrios Mesa
## Codigo: 201911563

R.version.string
## [1] "R version 4.2.1 (2022-06-23)"

## TALLER A

## Librerias a usar:

## usar la función p_load de pacman para instalar/llamar las librerías de la clase
require(pacman)

p_load(tidyverse, # funciones para manipular/limpiar conjuntos de datos.
       rio, # función import/export: permite leer/escribir archivos desde diferentes formatos. 
       skimr, # función skim: describe un conjunto de datos
       janitor, # función tabyl: frecuencias relativas
       dplyr) # contiene conjuntos de datos.

# Instalar paquete readr
install.packages("readr")

# Cargar paquete rear
library(readr)

## Punto 1: Vectores

# Vector con numero del 1 al 100
vector <- 1:100

# Vector con numeros impares del 1 al 99
vector_impares <- seq(1,99,by=2)

# Vector con numeros pares
vector_pares <- vector_impares + 1
