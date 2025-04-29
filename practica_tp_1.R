## ej 3
library(dplyr)
library(ggplot2)

datos <- read.csv("establecimientos_educativos.csv", header = T, sep=",", dec=".")

datos <- datos %>%
  select(establecimiento_id, establecimiento_nombre, modalidad, nivel, sector, dependencia, categoria, area, ambito, matricula, secciones, turnos)


# pregunta A

View(datos)

cant_establecimientos_categoria <- datos %>%
  group_by(categoria) %>%
  summarise(count=n()) %>%
  filter(!(categoria == "S/Datos"))

# eliminamos los sin datos (explicar porque)

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
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.005))) +
  labs(title = "Cantidad de establecimientos por ámbito", x = "Ámbito", y = "Cantidad de establecimientos")
