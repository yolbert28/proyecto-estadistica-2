# install.packages("ggthemes")

# Cargar las librerías necesarias
library(Lock5Data)    # Para acceder a la base de datos SleepStudy
library(dplyr)
library(tidyverse)    # Para manipulación de datos y visualización
library(moments)      # Para cálculo de asimetría y curtosis
library(gridExtra)    # Para organizar múltiples gráficos
library(ggthemes)     # Para estilos de gráficos adicionales

# Cargar los datos
data("SleepStudy")
datos <- SleepStudy

## 1. Medidas de tendencia central, dispersión y forma para variables cuantitativas

# Seleccionamos algunas variables cuantitativas clave
vars_cuantitativas <- c("GPA", "ClassesMissed", "CognitionZscore", "PoorSleepQuality",
                        "DepressionScore", "AnxietyScore", "StressScore", "DASScore",
                        "Happiness", "Drinks", "WeekdaySleep", "WeekendSleep", "WeekdayBed", "WeekdayRise", "AverageSleep")

# Función para calcular estadísticas descriptivas
estadisticas_descriptivas <- function(x) {
  x <- na.omit(x) # Eliminar NA's
  data.frame(
    Mínimo = min(x),
    Máximo = max(x),
    Media = mean(x),
    Mediana = median(x),
    Moda = names(which.max(table(x))),
    Desviacion = sd(x),
    Varianza = var(x),
    Rango_IQR = IQR(x),
    Asimetria = skewness(x),
    Curtosis = kurtosis(x)
  )
}

# Aplicamos la función a cada variable cuantitativa
estadisticas <- map_dfr(datos[vars_cuantitativas], estadisticas_descriptivas, .id = "Variable")

# Mostramos las estadísticas
print(estadisticas)

## 2. Distribuciones de frecuencia para variables cualitativas

vars_cualitativas <- c("Gender", "ClassYear", "LarkOwl", "EarlyClass", 
                       "DepressionStatus", "AnxietyStatus", "Stress", 
                       "AlcoholUse", "AllNighter")

# Función para generar tablas de frecuencia
tabla_frecuencias <- function(var_name) {
  datos %>%
    count(.data[[var_name]]) %>%
    mutate(Proporcion = n / sum(n) * 100) %>%
    rename(Categoria = 1)
}

# Generar tablas para cada variable cualitativa
frecuencias <- map(vars_cualitativas, tabla_frecuencias) %>%
  set_names(vars_cualitativas)

# Mostrar las tablas de frecuencia
walk2(frecuencias, names(frecuencias), 
      ~{
        cat("\nFrecuencias para:", .y, "\n")
        print(.x)
      })

## 3. Visualizaciones básicas (originales)

# Histogramas para variables cuantitativas
histogramas <- map(vars_cuantitativas, ~{
  ggplot(datos, aes(x = .data[[.x]])) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = paste("Histograma de", .x), x = .x, y = "Frecuencia") +
    theme_minimal()
})

# Diagramas de caja para variables cuantitativas por género
boxplots_genero <- map(vars_cuantitativas[1:6], ~{
  ggplot(datos, aes(x = factor(Gender), y = .data[[.x]], fill = factor(Gender))) +
    geom_boxplot() +
    scale_fill_discrete(name = "Género", labels = c("Mujer", "Hombre")) +
    labs(title = paste(.x, "por género"), x = "Género", y = .x) +
    theme_minimal()
})

# Gráficos de barras para variables cualitativas
graficos_barras <- map(vars_cualitativas[c(1,3,5,7,9)], ~{
  datos %>%
    ggplot(aes(x = .data[[.x]], fill = .data[[.x]])) +
    geom_bar() +
    labs(title = paste("Distribución de", .x), x = .x, y = "Conteo") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

# Gráficos de violín para algunas variables
violin_plots <- map(c("WeekdaySleep", "WeekendSleep", "AverageSleep"), ~{
  ggplot(datos, aes(x = factor(Gender), y = .data[[.x]], fill = factor(Gender))) +
    geom_violin(trim = FALSE) +
    geom_boxplot(width = 0.1, fill = "white") +
    scale_fill_discrete(name = "Género", labels = c("Mujer", "Hombre")) +
    labs(title = paste("Distribución de", .x, "por género"), 
         x = "Género", y = .x) +
    theme_minimal()
})

## 4. Gráficos adicionales solicitados

# 1. Gráfico de distribución de género
g_genero <- ggplot(datos, aes(x = factor(Gender, labels = c("Mujer", "Hombre")))) +
  geom_bar(fill = c("#FF9AA2", "#81dfde"), width = 0.7) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribución por Género", 
       x = "Género", y = "Cantidad de estudiantes") +
  theme_economist() +
  theme(legend.position = "none")

# 2. Distribución de cronotipo (LarkOwl)
g_cronotipo <- datos %>%
  count(LarkOwl) %>%
  mutate(LarkOwl = factor(LarkOwl, levels = c("Lark", "Neither", "Owl"))) %>%
  ggplot(aes(x = LarkOwl, y = n, fill = LarkOwl)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribución de Cronotipos", 
       x = "Cronotipo", y = "Cantidad de estudiantes",
       caption = "Lark = Alondra (madrugadora)\nOwl = Búho (nocturno)") +
  theme_minimal() +
  theme(legend.position = "none")

# 3. Presencia en clases antes de las 9 de la mañana
g_clases_tempranas <- ggplot(datos, aes(x = factor(EarlyClass, labels = c("No", "Sí")))) +
  geom_bar(fill = c("#ff5961", "#59cf70"), width = 0.6) +
  geom_text(
    aes(label = paste0(round(after_stat(count)/sum(after_stat(count))*100, 1), "%"),
        y = after_stat(count)/sum(after_stat(count))),
    stat = "count", 
    vjust = -0.5
  ) +
  labs(
    title = "Presencia en clases antes de las 9am",
    x = "Tiene clases antes de las 9am", 
    y = "Porcentaje"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()


# 4. histograma de clases perdidas
g_clases_perdidas <- ggplot(datos, aes(x = ClassesMissed)) +
  geom_histogram(binwidth = 1, fill = "#25943c", color = "white", alpha = 0.8) +
  scale_x_continuous(breaks = seq(0, max(datos$ClassesMissed), by = 1),
                     limits = c(-0.5, max(datos$ClassesMissed) + 0.5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribución de Clases Perdidas por Semestre",
       x = "Número de clases perdidas", 
       y = "Cantidad de estudiantes",
       caption = paste("Máximo observado:", max(datos$ClassesMissed), "clases")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"))

# 5. Consumo de alcohol
g_alcohol <- datos %>%
  count(AlcoholUse) %>%
  mutate(AlcoholUse = factor(AlcoholUse, 
                             levels = c("Abstain", "Light", "Moderate", "Heavy"))) %>%
  ggplot(aes(x = AlcoholUse, y = n, fill = AlcoholUse)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_fill_manual(values = c("#E2F0CB", "#B5EAD7", "#C7CEEA", "#FFDAC1")) +
  labs(title = "Consumo de Alcohol entre Estudiantes",
       x = "Nivel de consumo", y = "Cantidad de estudiantes") +
  theme_minimal() +
  theme(legend.position = "none")

# 6. Comparación de GPA por cronotipo (Diagrama de caja)
g_gpa_cronotipo <- datos %>%
  mutate(LarkOwl = factor(LarkOwl, levels = c("Lark", "Neither", "Owl"))) %>%
  ggplot(aes(x = LarkOwl, y = GPA, fill = LarkOwl)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "GPA según Cronotipo",
       x = "Cronotipo", y = "GPA",
       caption = "El punto rojo indica la media") +
  theme_minimal() +
  theme(legend.position = "none")



## 7. GPA vs. Hora de acostarse entre semana (WeekdayBed)
g1 <- ggplot(datos, aes(x = WeekdayBed, y = GPA)) +
  geom_point(aes(color = factor(Gender, labels = c("Mujer", "Hombre"))), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  scale_x_continuous(breaks = seq(20, 28, by = 1),
                     labels = function(x) paste0(x %% 24, ":00")) +
  scale_color_manual(values = c("#FF9AA2", "#A0E7E5")) +
  labs(title = "Relación entre GPA y Hora de Acostarse (días de semana)",
       x = "Hora de acostarse (24h)",
       y = "GPA",
       color = "Género") +
  theme_minimal() +
  theme(legend.position = "bottom")

## 8. GPA vs. Hora de levantarse entre semana (WeekdayRise)
g2 <- ggplot(datos, aes(x = WeekdayRise, y = GPA)) +
  geom_point(aes(color = LarkOwl), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  scale_x_continuous(breaks = seq(5, 14, by = 1),
                     labels = function(x) paste0(x %% 24, ":00")) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Relación entre GPA y Hora de Levantarse (días de semana)",
       x = "Hora de levantarse (24h)",
       y = "GPA",
       color = "Cronotipo") +
  theme_minimal() +
  theme(legend.position = "bottom")

## 9. GPA vs. Duración del sueño (WeekdaySleep)
g3 <- ggplot(datos, aes(x = WeekdaySleep, y = GPA)) +
  geom_point(aes(size = PoorSleepQuality, color = Stress), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  scale_color_manual(values = c("normal" = "#B5EAD7", "high" = "#FFB7B2")) +
  labs(title = "Relación entre GPA y Duración del Sueño",
       x = "Horas de sueño (días de semana)",
       y = "GPA",
       color = "Nivel de Estrés",
       size = "Calidad de Sueño\n(mayor = peor)") +
  theme_minimal()

## Mostrar todos los gráficos adicionales

g1
g2
g3
g_genero
g_cronotipo
g_clases_tempranas
g_clases_perdidas
g_gpa_cronotipo
g_alcohol

## Mostrar también los gráficos originales (selección)
# Histogramas
grid.arrange(grobs = histogramas[1:4], ncol = 2)

# Boxplots por género
grid.arrange(grobs = boxplots_genero[1:4], ncol = 2)

# Gráficos de violín
grid.arrange(grobs = violin_plots, ncol = 2)