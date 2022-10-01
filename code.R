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

## Punto 2: Importar/exportar bases de datos

## 2.1 Importar

# Importar las bases de datos requeridas

# Importar los archivos csv
caracteristicas_generales_personas <- read_delim("input/Enero - Cabecera - Caracteristicas generales (Personas).csv", 
                                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

ocupados <- read_delim("input/Enero - Cabecera - Ocupados.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

## 2.2 Exportar

# Exportar en formato rds los objetos cargados anteriores
export(x = caracteristicas_generales_personas , file="output/Características generales (Personas).rds")
export(x = ocupados , file="output/Ocupados.rds")

## Punto 3: Generar variables

# Sobre el objeto que contiene la base Ocupados, se genera una variable ocupado igual a 1
ocupados <- mutate(.data = ocupados, ocupado = 1)

# Sobre el objeto que contiene la base Caracteristicas Generales, se genera una variable joven igual a 1 si la persona tiene entre 18 y 28 años de edad
caracteristicas_generales_personas <- mutate(.data = caracteristicas_generales_personas, 
                                             joven = ifelse(18<=caracteristicas_generales_personas$P6040 & 
                                                              caracteristicas_generales_personas$P6040<=28, yes = 1, no = 0))
## Punto 4: Eliminar filas/columnas de un conjunto de datos

# Del objeto que contiene las Caracteristicas Generales, solo se dejan las observaciones para las personas entre 18 y 70 años de edad
caracteristicas_generales_personas_modificado <- caracteristicas_generales_personas %>% subset(18<caracteristicas_generales_personas$P6040 &
                                                caracteristicas_generales_personas$P6040<70)

# Del objeto que contiene las Caracteristicas Generales, se seleccionan las variables secuencia_p, orden, hogar, directorio, P6020, P6040, P6920, DPTO, fex_c_2011, ESC y MES
variables <- c("SECUENCIA_P", "ORDEN","HOGAR","DIRECTORIO", "P6020", "P6040", "P6090", 
               "DPTO", "fex_c_2011","ESC","MES")

caracteristicas_generales_personas_seleccionado <- caracteristicas_generales_personas_modificado %>%
                                                  select(all_of(variables))

# NOTA: La variable P6920 no existe, por lo cual se selecciona la variable P6090

# Del objeto que contiene la base Ocupados, se seleccionan las variables secuencia_p, orden, hogar, directorio, ocupado, INGLABO y P6050
variables_ocu <- c("SECUENCIA_P","ORDEN", "HOGAR", "DIRECTORIO","ocupado","INGLABO","P6450")

ocupados_seleccionado <- ocupados %>% select(all_of(variables_ocu))

# NOTA: La variable P6050 no existe, por lo cual se selecciona la variable P6450

## Punto 5: Combinar bases de datos

# Usando las variables secuencia_p, orden, hogar y directorio se unen en una única base de datos los objetos del punto anterior
# Usando full_join()
base_datos_combinada <- full_join(x = caracteristicas_generales_personas_seleccionado, 
                                   y = ocupados_seleccionado, 
                                   by = c("SECUENCIA_P", "ORDEN", "HOGAR", "DIRECTORIO"))

#Usando left_join()
base_datos_lj <- left_join(x = caracteristicas_generales_personas_seleccionado,
                        y = ocupados_seleccionado,
                        by = c("SECUENCIA_P", "ORDEN", "HOGAR", "DIRECTORIO"))

## Punto 6: Descriptivas de un conjunto de datos

# Tablas:

## 1) **summary** ofrece una descripción general(quartil, media, mediana) de la base de datos Ocupados
summary(ocupados[,c("INGLABO", "DPTO","P6450")])

## 2) **summary** ofrece una descripción general(quartil, media, mediana) de la base de datos Caracteristicas Generales
summary(caracteristicas_generales_personas[,c("P6020", "P6040","P6050", "DPTO")])

## 3) **sumarize_all** ofrece una descripción general(quartil, media, mediana) de la base de datos combinada
base_datos_lj %>% 
select(INGLABO,P6020,P6040) %>%
summarize_all(list(min, max, median, mean), na.rm = T)

## 4) **group_by()** toma un tibble/data.frame y lo convierte en un tibble agrupado, donde las operaciones son realizadas por grupo. 
base_datos_lj %>% 
select(INGLABO,P6020,P6040) %>% 
group_by(P6020) %>%  
summarise(promedio_inglabo = mean(INGLABO, na.rm = T),
            media_inglabo = median(INGLABO, na.rm = T),
            promedio_p6040 = mean(P6040, na.rm = T),
            media_p6040 = median(P6040, na.rm = T))

## 5) **group_by()** toma un tibble/data.frame y lo convierte en un tibble agrupado, donde las operaciones son realizadas por grupo. 
base_datos_lj %>% 
select(INGLABO,P6020,P6040) %>% 
group_by(P6040) %>%  
summarise(promedio_inglabo = mean(INGLABO, na.rm = T),
            media_inglabo = median(INGLABO, na.rm = T),
            promedio_p6020 = mean(P6020, na.rm = T),
            media_p6020 = median(P6020, na.rm = T))

# Graficas:

# `ggplot()`

# **"mapping" y "aes"** se usan para indicar las cordenadas de los datos.
ggplot(data = base_datos_lj , mapping = aes(x = P6040 , y = INGLABO))

# + geometry
ggplot(data = base_datos_lj , mapping = aes(x = P6040 , y = INGLABO)) +
  geom_point(col = "red" , size = 0.5)

# Guardar la grafica dentro de un objeto: 
graph_1 <- ggplot(data = base_datos_lj, 
                  mapping = aes( x = P6040 , y = INGLABO, group= as.character(P6020), color = as.factor(P6020))) +
            geom_point(size = 0.5)
graph_1

# Añadir atributos a este objeto
graph_1 + scale_color_manual(values = c("2"="red" , "1"="blue") , label = c("1"="Hombre" , "2"="Mujer") , name = "Sexo") +
          labs(x = "Edad", y = "Ingresos Laborales", title = "Edad VS Ingresos Laborales", subtitle = "Desagregados por sexo")


## **density chart:**
graph_2 <- filter(base_datos_lj, !is.na(P6450) & INGLABO < 1e+07 ) %>% 
            ggplot(data=. , mapping = aes(x = INGLABO, group = as.factor(P6450), fill = as.factor(P6450))) + 
            geom_density() 
graph_2

## se cambian los ejes
graph_2 <- graph_2  + 
            scale_fill_discrete(label = c("1"="Verbal" , "2"="Escrito", "9"="No informa/Conoce") , name = "Contrato") + 
            labs(x = "Ingresos" , y = "",
                title = "Ingresos menores a 10 SLMV",
                subtitle = "Desagregados por tipo de contrato")
graph_2

# `group_by()` `+` `ggplot()`** 

## summarize data
base_datos_lj %>% 
  group_by(P6020) %>% 
  summarise(Ingresos = mean(INGLABO, na.rm = T)) 

## plot
graph_3 <- base_datos_lj %>% 
  group_by(P6020) %>% 
  summarise(Ingresos = mean(INGLABO, na.rm = T)) %>% 
  ggplot(data=. , mapping = aes(x = as.factor(P6020) , y = Ingresos, fill = as.factor(P6020))) + 
  geom_bar(stat = "identity") 
graph_3

##se cambian los ejes y el theme:
graph_3 +
  scale_fill_manual(values = c("2"="red" , "1"="blue") , label = c("1"="Hombre" , "2"="Mujer") , name = "Sexo") +
  labs(x = "sexo", y = "ingresos", title = "Ingresos laborales por sexo") + 
  theme_classic()

# Grafico de ingresos laborales por departamento:
graph_4 <- ggplot(data = base_datos_lj , mapping = aes(x = DPTO , y = INGLABO, group= as.character(P6020), color = as.factor(P6020))) +
          geom_point(size = 0.5)

graph_4

# Añadir atributos a este objeto
graph_4 + scale_color_manual(values = c("2"="red" , "1"="blue") , label = c("1"="Hombre" , "2"="Mujer") , name = "Sexo") +
  labs(x = "Departamento", y = "Ingresos Laborales", title = "Ingresos Laborales por Departamento", 
       subtitle = "Desagregados por sexo")




