## ej 3
library(dplyr)
library(ggplot2)
library(patchwork) #agregar a bibliografia

datos <- read.csv("establecimientos_educativos.csv", header = T, sep=",", dec=".")

datos <- datos %>%
  select(establecimiento_id, establecimiento_nombre, modalidad, nivel, sector, dependencia, categoria, area, ambito, matricula, secciones, turnos)


# pregunta A

View(datos)

cant_establecimientos_categoria <- datos %>%
  group_by(categoria) %>%
  summarise(count=n())

View(cant_establecimientos_categoria)

cant_establecimientos_area <- datos %>%
  group_by(area) %>%
  summarise(count=n())

View(cant_establecimientos_area)

cant_establecimientos_ambito <- datos %>%
  group_by(ambito) %>%
  summarise(count=n())

View(cant_establecimientos_ambito)

# graficamos:

ggplot(data = cant_establecimientos_categoria, mapping = aes(x = categoria, y = count)) + 
  geom_col() + 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.4))) +
  labs(title = "Cantidad de establecimientos por categoría", x = "Categoría", y = "Cantidad de establecimientos")

ggplot(data = cant_establecimientos_area, mapping = aes(x = area, y = count)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  labs(title = "Cantidad de establecimientos por área", x = "Área", y = "Cantidad de establecimientos")

ggplot(data = cant_establecimientos_ambito, mapping = aes(x = ambito, y = count)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.0))) +
  labs(title = "Cantidad de establecimientos por ámbito", x = "Ámbito", y = "Cantidad de establecimientos")


#¿Cómo es la distribución de cantidad de establecimientos educativos en la PBA por
#categoría, área y ámbito?

#Explicar que creamos para esto esas tablas y graficos
#Vemos que la mayor cantidad de establecimientos corresponde a los de primera categoría, seguidos por los de la segunda y finalmente por los de Tercera.
#Con respecto a la distribución de conurbano e interior, vemos que las cantidaded son parecidas.
#Finalmente, si analizamos por ámbito, vemos que hay una gran diferencia entre la cantidad de establecimientos en el urbano con respecto a los correspondientes a 
#rural Agrupado y Rural disperso, que son similares entre sí. Finalmente, las de la categoría itinerante pertenecen a una clara minoría.


#B. ¿Existe alguna relación entre la matrícula y la cantidad de secciones de un
#establecimiento. ¿Qué ocurre si se analiza discriminando por modalidad, área,
#sector, etc.?

#FILTRAR SECCIONES = 0 A PARTIR DE ACA

p2 <- ggplot(data = datos, mapping = aes(x = matricula, y = secciones)) + 
  geom_point(alpha = 0.2)

#Ver label para eje x

ggplot(data = datos, mapping = aes(x = matricula, y = secciones)) +
  geom_point() +
  facet_wrap(facets = vars(modalidad)) +
  xlim(0, 4000) + 
  ylim(0, 200)

#distinguimos por area
ggplot(data = datos, mapping = aes(x = matricula, y = secciones)) + 
  geom_point(alpha = 0.2) +
  facet_wrap(facets = vars(area))

#Se mantiene la misma relacion. A mayor matricula, mayor numero de secciones

#Sector
ggplot(data = datos, mapping = aes(x = matricula, y = secciones)) + 
  geom_point(alpha = 0.2) +
  facet_wrap(facets = vars(sector))



#¿Cuántos estudiantes y cuántas secciones tienen en promedio los establecimientos
#de una misma dependencia? ¿Cómo es la variabilidad? En especial interesa analizar
#las dependencias que tengan más de 500 establecimientos bajo su órbita, mientras
#que el resto puede ser analizado en conjunto como una categoría “otro” (en referencia a otro tipo de dependencia).

datos_matricula_filtrados <- datos %>%
  filter(matricula < 4000)

conteo_dependencia <- datos %>%
  count(dependencia, name = "cantidad")

datos_con_categorias <- datos %>%
  left_join(conteo_dependencia, by = "dependencia") %>%  #Nos permite agregar la cantidad de establecimientos con esa dependencia para filtrar mas facil despues.
  mutate(dependencia_grupo = if_else(cantidad < 500, "Otro", dependencia)) #Filtramos con un if else y le agregamos la columna del grupo asignado

promedios <- datos_con_categorias %>%
  group_by(dependencia_grupo) %>%
  summarise(
    cantidad = n(),
    estudiantes = sum(matricula),
    total_secciones = sum(secciones),
    promedio_matricula = mean(matricula),
    desvio_matricula = sd(matricula),
    promedio_secciones = mean(secciones),
    desvio_secciones = sd(secciones) #Ver que secciones = 0 es dato faltante (son las especiales). 
  )

p1 <- ggplot(data = datos_con_categorias, mapping = aes(x = dependencia_grupo, y = matricula)) +
  geom_violin() +
  geom_point(data = promedios, mapping = aes(x = dependencia_grupo, y = promedio_matricula), color = "skyblue", size = 3)

p2 <- ggplot(data = datos_con_categorias, mapping = aes(x = dependencia_grupo, y = matricula)) +
  geom_violin() +
  ylim(0, 2000) + 
  geom_point(data = promedios, mapping = aes(x = dependencia_grupo, y = promedio_matricula), color = "skyblue", size = 3)

p1+p2

#Hacer esto mismo para secciones



