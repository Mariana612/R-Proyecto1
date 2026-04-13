# PREPARACION DE INFOMRACION
library(ggplot2)
library(tidyr)
library(plotly)


data <- read.csv("Student_Mental_health.csv")

## Cambio de nombre de las columnas
colnames(data) <- c("Timestamp", "Genero", "Edad", "Carrera", 
                     "Ano_Estudio", "CGPA", "Estado_Civil", 
                     "Depresion", "Ansiedad", "Panico", "Tratamiento")

# GRAFICOS UNIVARIABLES
##  PPRIMER --------------------------------------------------
##  Muestra qué tan prevalente es la depresión en la muestra


### Contar cuántos hay en cada grupo
conteos <- table(data$Depresion)

### Calcular porcentajes
porcentajes <- round(prop.table(conteos) * 100, 1)

### Crear Tabla
conteo_depresion <- data.frame(
  Depresion  = names(conteos),
  Cantidad   = as.numeric(conteos),
  Porcentaje = as.numeric(porcentajes),
  Etiqueta   = paste0(as.numeric(porcentajes), "%")
)

U1 <-ggplot(conteo_depresion, aes(x = Depresion, y = Cantidad, fill = Depresion,
                                 text = paste0("Cantidad: ", Cantidad, "\nPorcentaje: ", Porcentaje, "%"))) +
  geom_col(width = 0.5) +
  geom_text(aes(y = Cantidad +2, label = Etiqueta), size = 4) +
  ylim(0, max(conteo_depresion$Cantidad) * 1.15) + 
  scale_fill_manual(values = c("No" = "#F4C430", "Yes" = "#2D68C4")) +
  scale_x_discrete(labels = c("No" = "No", "Yes" = "Sí")) +
  labs(
    title = "Distribución de Depresión en Estudiantes Encuestados",
    x = "Presencia de Depresión en el Estudiante",
    y = "Número de Estudiantes Encuestados"
  )+
  theme(legend.position = "none")

ggplotly(U1, tooltip = "text")

##  SEGUNDO --------------------------------------------------
##  ¿En qué año hay más estudiantes con depresion?
data$CGPA <- trimws(data$CGPA)
U2<- ggplot(data)+
  geom_bar(aes(x=CGPA, fill = CGPA)) +
  scale_fill_manual(values = c("0 - 1.99" = "#F1B972", "2.00 - 2.49" = "#FED8B1",
                               "2.50 - 2.99" = "#FDAE44","3.00 - 3.49" = "#E77D22",
                               "3.50 - 4.00" = "#D16002")) +
  guides(x = guide_axis(angle = 30)) +
  labs(
  title = "Distribución de Estudiantes por Rendimiento Académico (CGPA)",
  x = "Rango de CGPA",
  y = "Número de Estudiantes") +
  theme(legend.position = "none")

ggplotly(U2)
##  TERCERO --------------------------------------------------
##  Distribucion de Depresion por genero
conteo_genero <- data.frame(
  Genero = names(table(data$Genero)),
  Cantidad = as.numeric(table(data$Genero))
)

U3 <- plot_ly(conteo_genero, 
              labels = ~Genero, 
              values = ~Cantidad,
              type = "pie",
              textinfo = "label+percent",
              hoverinfo = "text",
              text = ~paste0(Genero, ": ", Cantidad, " estudiantes"),
              marker = list(colors = c("#DC9DDD", "#4682B4"))) %>%
  layout(title = "Distribución de Estudiantes por Género")

U3

# ================================================================
# Grafico Bi-Variable
## PRIMERO--------------------------------------------------
## Cantidad de Estudiantes con depresion vs CGPA
Bi1 <- ggplot(data, aes(x = CGPA, fill = Depresion)) + 
  geom_bar(position = "dodge") +
  labs(
    title = "Relación entre CGPA y Depresión",
    x = "Rango de CGPA",
    y = "Número de Estudiantes"
  ) +
  scale_fill_manual(values = c("No" = "#F4C430", "Yes" = "#2D68C4")) +
  guides(x = guide_axis(angle = 30)) 

ggplotly(Bi1)

## SEGUNDO--------------------------------------------------
## Ansiedad vs Ano de Estudio

data$Ano_Estudio <- tolower(trimws(data$Ano_Estudio))

# Calcular conteos directamente
conteo_Bi2 <- as.data.frame(table(
  Anio = data$Ano_Estudio,
  Ansiedad = data$Ansiedad ))

# Graficar
Bi2 <- ggplot(conteo_Bi2, aes(x = Anio, y = Freq, fill = Ansiedad,
                               text = paste0("Año: ", Anio, "\nAnsiedad: ", Ansiedad, "\nCantidad: ", Freq))) +
  geom_col(position = "dodge", width = 0.6) +
  labs(
    title = "Relación entre Ansiedad y Año de Estudio",
    x = "Año de Estudio",
    y = "Número de Estudiantes"
  ) +
  scale_x_discrete(labels = c("year 1" = "Año 1", "year 2" = "Año 2",
                              "year 3" = "Año 3", "year 4" = "Año 4")) +
  scale_fill_manual(values = c("No" = "#F4C430", "Yes" = "#2D68C4"),
                    labels = c("No" = "Sin Ansiedad", "Yes" = "Con Ansiedad")) +
  theme_minimal()

ggplotly(Bi2, tooltip = "text")

## TERCERO--------------------------------------------------
#Depresion Vs Genero 
Bi3<-ggplot(data, aes(x = Genero, fill = Depresion)) +
  geom_bar(position = "fill") +  # fill = 100%
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("No" = "#F4C430", "Yes" = "#2D68C4")) +
  labs(title = "Depresión por Género", x = "Género", y = "Porcentaje")
ggplotly(Bi3)


# ================================================================
##Grafico multivariable

data_par <- data.frame(
  Edad        = data$Edad,
  Depresion   = ifelse(data$Depresion == "Yes", 1, 0),
  Ansiedad    = ifelse(data$Ansiedad == "Yes", 1, 0),
  Panico      = ifelse(data$Panico == "Yes", 1, 0),
  Tratamiento = ifelse(data$Tratamiento == "Yes", 1, 0),
  CGPA        = as.numeric(factor(data$CGPA, 
                                  levels = c("0 - 1.99", "2.00 - 2.49", "2.50 - 2.99",
                                             "3.00 - 3.49", "3.50 - 4.00")))
)

plot_ly(data_par, type = "parcoords",
        line = list(
          color = ~Depresion,
          colorscale = list(c(0, "#F4C430"), c(1, "#2D68C4"))
        ),
        dimensions = list(
          list(label = "Edad",        values = ~Edad),
          list(label = "CGPA",        values = ~CGPA,
               tickvals = 1:5,
               ticktext = c("0-1.99","2.00-2.49","2.50-2.99","3.00-3.49","3.50-4.00")),
          list(label = "Depresión",   values = ~Depresion,
               tickvals = c(0,1), ticktext = c("No","Sí")),
          list(label = "Ansiedad",    values = ~Ansiedad,
               tickvals = c(0,1), ticktext = c("No","Sí")),
          list(label = "Pánico",      values = ~Panico,
               tickvals = c(0,1), ticktext = c("No","Sí")),
          list(label = "Tratamiento", values = ~Tratamiento,
               tickvals = c(0,1), ticktext = c("No","Sí"))
        )
) %>%
  layout(title = "Perfil Multidimensional del Estudiante")


# ================================================================
## Grafico con facetas

# Convertir a formato largo
data_largo <- data %>%
  select(Genero, Depresion, Ansiedad, Panico) %>%
  pivot_longer(
    cols = c(Depresion, Ansiedad, Panico),
    names_to = "Condicion",
    values_to = "Valor"
  )


g_f1 <- ggplot(data_largo, aes(x = Condicion, fill = Valor,
                               text = paste0("Condición: ", Condicion,
                                             "\nRespuesta: ", Valor))) +
  geom_bar(position = "fill") +
  facet_wrap(~Genero,                                   # <-- aquí están las facetas
             labeller = labeller(Genero = c("Male" = "Hombres", 
                                            "Female" = "Mujeres"))) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("No" = "#F4C430", "Yes" = "#2D68C4"),
                    labels = c("No" = "No", "Yes" = "Sí")) +
  scale_x_discrete(labels = c("Depresion" = "Depresión",
                              "Ansiedad"  = "Ansiedad",
                              "Panico"    = "Pánico")) +
  labs(
    title = "Condiciones de Salud Mental por Género",
    x     = "Condición",
    y     = "Porcentaje",
    fill  = "Presenta"
  ) +
  theme_minimal()

ggplotly(g_f1, tooltip = "text")




# Limpiar títulos Y etiquetas de ejes de cada gráfico
p1 <- ggplotly(U1 + labs(title = "", x = "", y = ""), tooltip = "text")
p2 <- ggplotly(U2 + labs(title = "", x = "", y = ""))
p3 <- ggplotly(Bi2 + labs(title = "", x = "", y = ""), tooltip = "text")
p4 <- ggplotly(Bi3 + labs(title = "", x = "", y = ""))

compuesta <- subplot(
  p1, p2, p3, p4,
  nrows  = 2,
  shareX = FALSE,
  shareY = FALSE,
  titleX = FALSE,   # <-- desactivar títulos de ejes heredados
  titleY = FALSE,
  margin = 0.10
) %>%
  layout(
    title = list(
      text = "<b>Resumen del Análisis de Salud Mental Estudiantil</b>",
      font = list(size = 15),
      x    = 0.5
    ),
    showlegend = FALSE,
    margin = list(t = 100, b = 60, l = 50, r = 30),
    annotations = list(
      list(x = 0.22, y = 1.00, text = "<b>Depresión en Estudiantes</b>",
           xref = "paper", yref = "paper", showarrow = FALSE,
           font = list(size = 11), xanchor = "center"),
      list(x = 0.78, y = 1.00, text = "<b>Distribución por CGPA</b>",
           xref = "paper", yref = "paper", showarrow = FALSE,
           font = list(size = 11), xanchor = "center"),
      list(x = 0.22, y = 0.44, text = "<b>Ansiedad por Año de Estudio</b>",
           xref = "paper", yref = "paper", showarrow = FALSE,
           font = list(size = 11), xanchor = "center"),
      list(x = 0.78, y = 0.44, text = "<b>Depresión por Género</b>",
           xref = "paper", yref = "paper", showarrow = FALSE,
           font = list(size = 11), xanchor = "center")
    )
  )

compuesta