library(gridExtra)
source("/Users/ftoresh/Google Drive/Data Analysis/Script Formulas/Initiate.R")

### Creación de data----

## Productos
VectorProductos <- 
  c("Producto A", "Producto B", "Producto C")

dataset_categorico <-
  tibble(
    Producto = rep(VectorProductos, 100),
    Ventas = runif(300, min = 0, max = 100),
    Costo.Venta = runif(300, min = 0, max = 10),
    Fecha = seq(as.Date("2019/01/01"), as.Date("2019/10/27"), "days")
  ) %>%
  mutate(Costo.Venta.Mod = case_when(Producto == "Producto A" ~ Costo.Venta *
                                       1.9,
                                     TRUE ~ Costo.Venta),
         Categoria = case_when(Costo.Venta.Mod <= 5 ~ "Nacional",
                               Costo.Venta.Mod > 10 ~ "Importado",
                               TRUE ~ "Importado/Nacional")) %>%
  mutate(Mes = month(Fecha))
      
write.csv(dataset_categorico, "dataset_ventas")

# ## Vector de fechas para crear el Big dataset
# VectorFecha <- 
#   seq(as.Date("2019/01/01"), as.Date("2019/10/27"), "days")
#   
# dataset_categorico_biggest <- 
#   tibble(
#     Producto = rep(VectorProductos, 300000),
#     Ventas = runif(900000, min = 0, max = 100),
#     Costo.Venta = runif(900000, min = 0, max = 10),
#     Fecha = rep(VectorFecha, 3000)
#   ) %>%
#   mutate(Costo.Venta.Mod = case_when(Producto == "Producto A" ~ Costo.Venta *
#                                        1.5,
#                                      TRUE ~ Costo.Venta)) %>%
#   mutate(Mes = month(Fecha)) %>% 
#   sample_n(80000)


### Creación de charts----


# Relación precio, costo de venta
dataset_categorico %>% 
  group_by(Producto) %>% 
  ggplot(aes(Ventas, Costo.Venta.Mod, colour = Producto)) + 
  geom_point() +
  theme_minimal() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  labs(title = "Gráfico de Puntos Por Producto",
       subtitle = "Producto A genera similar ventas que B y C, pero su costo de venta es mayor",
       x = "Ventas",
       y = "Costo de Venta")
 

# Ventas por producto barras
dataset_categorico %>% 
  group_by(Producto) %>% 
  summarise(Total.Ventas.Producto = sum(Ventas)) %>% 
  ggplot(aes(Producto, Total.Ventas.Producto, fill = Producto)) + 
  geom_col(alpha = 0.8) +
  theme_minimal() +
  scale_fill_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12)) +
  labs(title = "Ventas Totales Por Producto",
       subtitle = "Producto C genera mayores ingresos",
       x = "Producto",
       y = "Total Ventas") 


# Ventas por producto barras stacked
dataset_categorico %>% 
  group_by(Categoria, Producto) %>% 
  summarise(Costo.Venta.Mod = sum(Ventas)) %>% 
  ungroup() %>% 
  ggplot(aes(Categoria, Costo.Venta.Mod)) + 
  geom_col(aes(fill = Producto), alpha = 0.9) +
  theme_minimal() +
  scale_fill_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12)) +
  labs(title = "Costo de Venta Totales Por Producto",
       x = "Categoría",
       y = "Costo de Venta") 
  

# Ventas por producto Pie
dataset_categorico %>%
  group_by(Producto) %>%
  summarise(Total.Ventas.Producto = sum(Ventas)) %>%
  ggplot(aes("", Total.Ventas.Producto, fill = Producto)) +
  geom_col(alpha = 0.8) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  scale_fill_economist() +
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12)
  ) +
  labs(title = "Ventas Totales Por Producto",
       subtitle = "Producto ¿A, B, C? genera mayores ingresos",
       y = "Ventas")


# Histograma de costo de venta
dataset_categorico %>% 
  ggplot(aes(Costo.Venta.Mod)) +
  geom_histogram(bins = 15, fill = "deepskyblue3", alpha = 0.8) +
  theme_minimal() +
  scale_fill_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  labs(title = "Diferentes Costos de Venta Por Producto",
       subtitle = "Producto A tiene costos de ventas por encima de $10",
       x = "Costo de Venta",
       y = "Número") +
  facet_grid(~ Producto)
  

### Series de tiempo----

### Design a chart

# Serie de tiempo diaria
## Saturación, tendencia no clara, estacionalidad no clara
dataset_categorico %>% 
  ggplot(aes(Fecha, Ventas, colour = Producto)) +
  geom_line(aes(group = Producto)) +
  theme_minimal() +
  scale_colour_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  labs(title = "Ventas a lo largo del año por producto",
       subtitle = "Producto ¿A , B, C? tiene un decrecimiento en ventas")


# Serie de tiempo mensual I
## Saturación, lineas no claras
## Puntos/Líneas
dataset_categorico %>%
  group_by(Mes, Producto) %>%
  mutate(Total.Ventas.Mes = sum(Ventas)) %>%
  ungroup() %>% 
  group_by(Producto) %>% 
  distinct(Mes, .keep_all = TRUE) %>% 
  ggplot(aes(Fecha, Total.Ventas.Mes, colour = Producto)) +
  geom_line(aes(group = Producto), alpha = 0.8) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  scale_colour_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  labs(title = "Ventas a lo largo del año por producto",
       subtitle = "Crecimiento por producto no claro",
       y = "Ventas") 


# Serie de tiempo mensual II
## Saturación, lineas no claras, tendencias no claras
## Puntos/Líneas/Tendencia Loess
dataset_categorico %>%
  group_by(Mes, Producto) %>%
  mutate(Total.Ventas.Mes = sum(Ventas)) %>%
  ungroup() %>% 
  group_by(Producto) %>% 
  distinct(Mes, .keep_all = TRUE) %>% 
  ggplot(aes(Fecha, Total.Ventas.Mes, colour = Producto)) +
  geom_line(aes(group = Producto), alpha = 0.8) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess") +
  # geom_smooth(se = FALSE, method = "lm") +
  theme_minimal() +
  scale_colour_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  labs(title = "Ventas a lo largo del año por producto",
       subtitle = "Crecimiento por producto no claro",
       y = "Ventas") 


# Serie de tiempo mensual III
## Tendencia estacional clara pero crecimiento por producto no claro
## Puntos / Tendencia Loess
dataset_categorico %>%
  group_by(Mes, Producto) %>%
  mutate(Total.Ventas.Mes = sum(Ventas)) %>%
  ungroup() %>% 
  group_by(Producto) %>% 
  distinct(Mes, .keep_all = TRUE) %>% 
  ggplot(aes(Fecha, Total.Ventas.Mes, colour = Producto)) +
  # geom_line(aes(group = Producto), alpha = 0.8) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess") +
  # geom_smooth(se = FALSE, method = "lm") +
  theme_minimal() +
  scale_colour_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  labs(title = "Ventas a lo largo del año por producto",
       subtitle = "Estacionalidad clara, crecimiento por producto no claro",
       y = "Ventas") 



# Serie de tiempo mensual IV
## Saturación, lineas no claras, tendencias claras pero gráfico saturado
## Puntos/Líneas/Tendencia Lineal
dataset_categorico %>%
  group_by(Mes, Producto) %>%
  mutate(Total.Ventas.Mes = sum(Ventas)) %>%
  ungroup() %>% 
  group_by(Producto) %>% 
  distinct(Mes, .keep_all = TRUE) %>% 
  ggplot(aes(Fecha, Total.Ventas.Mes, colour = Producto)) +
  geom_line(aes(group = Producto), alpha = 0.8) +
  geom_point(alpha = 0.7) +
  # geom_smooth(se = FALSE, method = "loess") +
  geom_smooth(se = FALSE, method = "lm") +
  theme_minimal() +
  scale_colour_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  labs(title = "Ventas a lo largo del año por producto",
       subtitle = "Crecimiento por producto claro ¡Gráfico saturado!",
       y = "Ventas")




# Serie de tiempo mensual V
## Crecimiento por producto clara pero no tendencia estacionaria
## Puntos / Tendencia lineal
dataset_categorico %>%
  group_by(Mes, Producto) %>%
  mutate(Total.Ventas.Mes = sum(Ventas)) %>%
  ungroup() %>% 
  group_by(Producto) %>% 
  distinct(Mes, .keep_all = TRUE) %>%  
  ggplot(aes(Fecha, Total.Ventas.Mes, colour = Producto)) +
  # geom_line(aes(group = Producto), alpha = 0.8) +
  geom_point(alpha = 0.7) +
  # geom_smooth(se = FALSE, method = "loess") +
  geom_smooth(se = FALSE, method = "lm") +
  theme_minimal() +
  scale_colour_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  labs(title = "Ventas a lo largo del año por producto",
       subtitle = "Crecimiento por producto claro",
       y = "Ventas")


# Serie de tiempo mensual VI
## Crecimiento por producto no clara pero si tendencia estacionaria
## Puntos / Tendencia loess
dataset_categorico %>%
  group_by(Mes, Producto) %>%
  mutate(Total.Ventas.Mes = sum(Ventas)) %>%
  ungroup() %>% 
  group_by(Producto) %>% 
  distinct(Mes, .keep_all = TRUE) %>%  
  ggplot(aes(Fecha, Total.Ventas.Mes, colour = Producto)) +
  # geom_line(aes(group = Producto), alpha = 0.8) +
  geom_point(alpha = 0.7) +
  # geom_smooth(se = FALSE, method = "loess") +
  geom_smooth(se = FALSE, method = "loess") +
  theme_minimal() +
  scale_colour_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  labs(title = "Ventas a lo largo del año por producto",
       subtitle = "Crecimiento por producto claro",
       y = "Ventas")





### Tendencias por volumen de datos----

### Bigger

plotTendLineal <- 
  dataset_categorico %>%
  group_by(Mes, Producto) %>%
  mutate(Total.Ventas.Mes = sum(Ventas)) %>%
  ungroup() %>% 
  group_by(Producto) %>% 
  distinct(Mes, .keep_all = TRUE) %>%  
  ggplot(aes(Fecha, Total.Ventas.Mes, colour = Producto)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "lm") +
  theme_minimal() +
  scale_colour_economist() +
  theme(plot.title = element_text(size=22),
        legend.title = element_blank(),
        plot.subtitle = element_text(size=15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  labs(y = "Ventas")



# Lineal
plotTendLinealBigger <- 
  dataset_categorico %>%
  ggplot(aes(Fecha, Ventas, colour = Producto)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "lm") +
  theme_minimal() +
  scale_colour_economist() +
  theme(plot.title = element_text(size=22),
        legend.title = element_blank(),
        plot.subtitle = element_text(size=15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_blank())

grid.arrange(plotTendLineal, plotTendLinealBigger, nrow=1, ncol=2)



# Non linear

plotTendLoess <- 
  dataset_categorico %>%
  group_by(Mes, Producto) %>%
  mutate(Total.Ventas.Mes = sum(Ventas)) %>%
  ungroup() %>% 
  group_by(Producto) %>% 
  distinct(Mes, .keep_all = TRUE) %>% 
  ggplot(aes(Fecha, Total.Ventas.Mes, colour = Producto)) +
  # geom_line(aes(group = Producto), alpha = 0.8) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess") +
  # geom_smooth(se = FALSE, method = "lm") +
  theme_minimal() +
  scale_colour_economist() +
  theme(plot.title = element_text(size=22),
        legend.title = element_blank(),
        plot.subtitle = element_text(size=15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  labs(y = "Ventas")


plotTendLoessBigger <- dataset_categorico %>%
  ggplot(aes(Fecha, Ventas, colour = Producto)) +
  # geom_line(aes(group = Producto), alpha = 0.8) +
  geom_point(alpha = 0.7) +
  # geom_smooth(se = FALSE, method = "loess") +
  geom_smooth(se = FALSE, method = "loess") +
  theme_minimal() +
  scale_colour_economist() +
  theme(plot.title = element_text(size=22),
        legend.title = element_blank(),
        plot.subtitle = element_text(size=15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_blank())

grid.arrange(plotTendLoess, plotTendLoessBigger, nrow=1, ncol=2)



### Story Telling----

# Hipotesis Inicial: Producto A debe salir del mercado porque genera un mayor costo de ventas
# Hipotesis Final: Productos Importados son los que generan mayor costo de venta sin que esto signifique 
                 # mayores ventas


### Exploración

# Costo de venta por producto barras
## Producto A es el que genera mayores costos
dataset_categorico %>% 
  group_by(Producto) %>% 
  summarise(Total.Costo.Venta = sum(Costo.Venta.Mod)) %>% 
  ggplot(aes(Producto, Total.Costo.Venta, fill = Producto)) + 
  geom_col(alpha = 0.8) +
  theme_minimal() +
  scale_fill_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  labs(title = "Costo de Venta Total Por Producto",
       subtitle = "Producto A genera mayores costos de venta",
       x = "Producto",
       y = "Costo de Venta") 


# Ventas por producto barras
## Las ventas de Producto A no compensan su costo
dataset_categorico %>% 
  group_by(Producto) %>% 
  summarise(Total.Ventas.Producto = sum(Ventas)) %>% 
  ggplot(aes(Producto, Total.Ventas.Producto, fill = Producto)) + 
  geom_col(alpha = 0.8) +
  theme_minimal() +
  scale_fill_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  labs(title = "Ventas Totales Por Producto",
       subtitle = "Producto C genera mayores ingresos, seguido de A y B",
       x = "Producto",
       y = "Ventas") 


# Relación precio, costo de venta
## Hipotesis falsa: Producto A efectivamente parece tener un mayor costo
dataset_categorico %>% 
  group_by(Producto) %>% 
  ggplot(aes(Ventas, Costo.Venta.Mod, colour = Producto)) + 
  geom_point() +
  theme_minimal() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  labs(title = "Gráfico de Puntos Por Producto",
       subtitle = "Producto A genera similar ventas que B y C, pero su costo de venta es mayor",
       x = "Ventas",
       y = "Costo de Venta")



# Total Costo de venta por categoría
## Ver si es producto A o es la categoría la que genera mayores costos
## Hipotesis falsa: Productos importados no tienen un gran costo de venta
dataset_categorico %>% 
  group_by(Categoria) %>% 
  summarise(Total.Costo.Venta = sum(Costo.Venta.Mod)) %>% 
  ggplot(aes(Categoria, Total.Costo.Venta, fill = Categoria)) + 
  geom_col(alpha = 0.8) +
  theme_minimal() +
  scale_fill_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  labs(title = "Costo de Venta Total Por Categoría",
       subtitle = "Productos importados no generan los mayores costos de ventas",
       x = "Categoría",
       y = "Costo de Venta") 




# Relación precio, costo de venta por categoría
## Ahora parece que productos importados si son el problema
dataset_categorico %>% 
  group_by(Categoria) %>% 
  ggplot(aes(Ventas, Costo.Venta.Mod, colour = Categoria)) + 
  geom_point() +
  theme_minimal() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  labs(title = "Gráfico de Puntos Por Categoría",
       subtitle = "Productos importados tienen mayores costos de venta",
       x = "Ventas",
       y = "Costo de Venta")



# Costo de venta por producto y categoría stacked
## No se puede culpar a producto A porque este está en las tres categorías
## Parece que producto A si tiene un costo de venta alto en Importado
dataset_categorico %>% 
  group_by(Categoria, Producto) %>% 
  summarise(Costo.Venta.Mod = sum(Ventas)) %>% 
  ungroup() %>% 
  ggplot(aes(Categoria, Costo.Venta.Mod)) + 
  geom_col(aes(fill = Producto), alpha = 0.9) +
  theme_minimal() +
  scale_fill_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  labs(title = "Costo de Venta Por Producto Proporcional a Categoría",
       subtitle = "Categoría A sólo incluye Producto A",
       x = "Categoría",
       y = "Costo de Venta") 



# Relación precio, costo de venta producto, face categoría
## Producto A importado efectivamente tiene un costo de venta más alto
dataset_categorico %>% 
  group_by(Producto) %>% 
  ggplot(aes(Ventas, Costo.Venta.Mod, colour = Producto)) + 
  geom_point() +
  theme_minimal() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  labs(title = "Gráfico de Puntos Por Producto y Categoría",
       subtitle = "Producto A importado tiene un costo de venta mayor",
       x = "Ventas",
       y = "Costo de Venta") +
  facet_grid(~ Categoria)


# Ratio costo venta/venta por producto y categoría
## Producto A importado si tiene un costo de venta alto, pero Producto C Importado/Nacional
## es el que tiene el mayor costo en relación a sus ventas
dataset_categorico %>%
  mutate(ratio.costo.venta = Costo.Venta.Mod / Ventas) %>%
  group_by(Categoria, Producto) %>%
  mutate(promedio.ratio.costo.venta = mean(ratio.costo.venta)) %>%
  ungroup() %>% 
  ggplot(aes(Categoria, promedio.ratio.costo.venta, fill = Producto)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() +
  scale_fill_economist() +
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  labs(title = "Comparación de rentabilidad por producto y categoría",
       subtitle = "Producto C Importado/Nacional es el menor rentable",
       x = "Categoría",
       y = "Proporción Costo de Venta / Ventas (%)") 





