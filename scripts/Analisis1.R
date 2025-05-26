# Cargar librerías necesarias
# install.packages("moments")
library(Lock5Data)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(purrr)
library(car)
library(nortest)
library(ggpubr)
library(moments)

#1. Carga y preparacion de datos

# Cargar los datos
data("SleepStudy")
datos <- SleepStudy

# Verificar estructura de los datos
str(datos)
summary(datos)

# Convertir variables categóricas a factores
datos <- datos %>%
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Mujer", "Hombre")),
         ClassYear = factor(ClassYear),
         LarkOwl = factor(LarkOwl, levels = c("Lark", "Neither", "Owl")),
         EarlyClass = factor(EarlyClass),
         DepressionStatus = factor(DepressionStatus, levels = c("normal", "moderate", "severe")),
         AnxietyStatus = factor(AnxietyStatus, levels = c("normal", "moderate", "severe")),
         Stress = factor(Stress, levels = c("normal", "high")),
         AlcoholUse = factor(AlcoholUse, levels = c("Abstain", "Light", "Moderate", "Heavy")),
         AllNighter = factor(AllNighter))








#2. Análisis descriptivo

# Función para resumen descriptivo
resumen_descriptivo <- function(variable, nombre) {
  media <- mean(variable, na.rm = TRUE)
  mediana <- median(variable, na.rm = TRUE)
  sd <- sd(variable, na.rm = TRUE)
  asimetria <- skewness(variable, na.rm = TRUE)
  curtosis <- kurtosis(variable, na.rm = TRUE)
  
  data.frame(
    Variable = nombre,
    Media = media,
    Mediana = mediana,
    Desviación = sd,
    Asimetría = asimetria,
    Curtosis = curtosis
  )
}

# Variables continuas de interés
variables_interes <- c("GPA", "WeekdaySleep", "WeekendSleep", "AverageSleep", 
                       "PoorSleepQuality", "DepressionScore", "AnxietyScore", 
                       "StressScore", "Happiness", "Drinks")

# Resumen descriptivo por género
descrip_hombres <- datos %>%
  filter(Gender == "Hombre") %>%
  select(all_of(variables_interes)) %>%
  map_df(~resumen_descriptivo(., deparse(substitute(.))))

descrip_mujeres <- datos %>%
  filter(Gender == "Mujer") %>%
  select(all_of(variables_interes)) %>%
  map_df(~resumen_descriptivo(., deparse(substitute(.))))

# Mostrar resultados
print("Resumen descriptivo para hombres:")
print(descrip_hombres)

print("Resumen descriptivo para mujeres:")
print(descrip_mujeres)






#3. Visualización de datos

# Histogramas para variables continuas
for (var in variables_interes) {
  print(
    ggplot(datos, aes_string(x = var, fill = "Gender")) +
      geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
      labs(title = paste("Distribución de", var), x = var, y = "Frecuencia") +
      theme_minimal()
  )
}

# Boxplots comparativos por género
for (var in variables_interes) {
  print(
    ggplot(datos, aes_string(x = "Gender", y = var, fill = "Gender")) +
      geom_boxplot() +
      labs(title = paste("Comparación de", var, "por género"), x = "Género", y = var) +
      theme_minimal()
  )
}






#4. inferencia estadística

# Función para verificar normalidad
verificar_normalidad <- function(variable, nombre) {
  # Gráfico Q-Q
  qqPlot(variable, main = paste("Gráfico Q-Q para", nombre))
  
  # Test de Shapiro-Wilk
  shapiro_test <- shapiro.test(variable)
  
  # Test de Anderson-Darling
  ad_test <- ad.test(variable)
  
  list(
    Variable = nombre,
    Shapiro = shapiro_test,
    Anderson_Darling = ad_test
  )
}

# Verificar normalidad para cada variable por género
norm_hombres <- datos %>%
  filter(Gender == "Hombre") %>%
  select(all_of(variables_interes)) %>%
  map(~verificar_normalidad(., deparse(substitute(.))))

norm_mujeres <- datos %>%
  filter(Gender == "Mujer") %>%
  select(all_of(variables_interes)) %>%
  map(~verificar_normalidad(., deparse(substitute(.))))







#Comparacion de medias entre generos

# Función corregida para comparar medias
comparar_medias <- function(var_name, datos) {
  # Extraer la variable
  variable <- datos[[var_name]]
  
  # Verificar homogeneidad de varianzas
  var_test <- leveneTest(variable ~ Gender, data = datos)
  
  # Realizar pruebas de normalidad por grupo
  shapiro_hombres <- shapiro.test(variable[datos$Gender == "Hombre"])
  shapiro_mujeres <- shapiro.test(variable[datos$Gender == "Mujer"])
  
  # Decidir qué test usar
  if (shapiro_hombres$p.value > 0.05 && shapiro_mujeres$p.value > 0.05) {
    # Si ambos grupos son normales, usar t-test
    if (var_test$`Pr(>F)`[1] > 0.05) {
      test <- t.test(variable ~ Gender, data = datos, var.equal = TRUE)
      metodo <- "t-test (varianzas iguales)"
    } else {
      test <- t.test(variable ~ Gender, data = datos, var.equal = FALSE)
      metodo <- "t-test (varianzas desiguales)"
    }
    ci <- test$conf.int
    diferencia <- diff(tapply(variable, datos$Gender, mean, na.rm = TRUE))
  } else {
    # Si no son normales, usar Wilcoxon
    test <- wilcox.test(variable ~ Gender, data = datos, conf.int = TRUE)
    metodo <- "Wilcoxon rank-sum test"
    ci <- test$conf.int
    diferencia <- median(variable[datos$Gender == "Hombre"], na.rm = TRUE) - 
      median(variable[datos$Gender == "Mujer"], na.rm = TRUE)
  }
  
  list(
    Variable = var_name,
    Metodo = metodo,
    P_value = test$p.value,
    Intervalo_Confianza = ci,
    Diferencia_Medias = diferencia
  )
}

# Aplicar la función corregida a todas las variables de interés
resultados_hipotesis <- map(variables_interes, ~comparar_medias(., datos))

# Mostrar resultados
for (res in resultados_hipotesis) {
  cat("\nVariable:", res$Variable, "\n")
  cat("Método:", res$Metodo, "\n")
  cat("Valor p:", res$P_value, "\n")
  cat("Intervalo de confianza:", res$Intervalo_Confianza, "\n")
  cat("Diferencia (Hombres - Mujeres):", res$Diferencia_Medias, "\n")
  cat("Conclusión:", ifelse(res$P_value < 0.05, 
                            "Hay diferencia significativa entre géneros", 
                            "No hay diferencia significativa entre géneros"), "\n")
}




#ANOVA para Comparar Más de Dos Grupos (ej. AlcoholUse)

# Ejemplo con AverageSleep y AlcoholUse
# Verificar normalidad por grupo
datos %>%
  group_by(AlcoholUse) %>%
  summarise(p_value = shapiro.test(AverageSleep)$p.value)

# Si no se cumple normalidad, usar Kruskal-Wallis
kruskal.test(AverageSleep ~ AlcoholUse, data = datos)

# Si hay diferencias significativas, hacer comparaciones por pares
pairwise.wilcox.test(datos$AverageSleep, datos$AlcoholUse, p.adjust.method = "BH")





#5. Recomendaciones por genero


# Función para generar recomendaciones
generar_recomendaciones <- function(resultados) {
  cat("\nRECOMENDACIONES BASADAS EN EL ANÁLISIS:\n\n")
  
  # Recomendaciones para hombres
  cat("PARA HOMBRES:\n")
  if (resultados[[5]]$P_value < 0.05 && 
      resultados[[5]]$Diferencia_Medias > 0) {
    cat("- Los hombres reportan significativamente peor calidad de sueño. Se recomienda:\n")
    cat("  * Establecer una rutina de sueño consistente\n")
    cat("  * Evitar el uso de pantallas antes de dormir\n")
    cat("  * Considerar evaluación para trastornos del sueño\n")
  }
  
  if (resultados[[10]]$P_value < 0.05 && 
      resultados[[10]]$Diferencia_Medias > 0) {
    cat("- Los hombres consumen significativamente más alcohol. Se recomienda:\n")
    cat("  * Moderar el consumo de alcohol, especialmente cerca de la hora de dormir\n")
    cat("  * Buscar alternativas sociales que no involucren alcohol\n")
  }
  
  # Recomendaciones para mujeres
  cat("\nPARA MUJERES:\n")
  if (resultados[[7]]$P_value < 0.05 && 
      resultados[[7]]$Diferencia_Medias < 0) {
    cat("- Las mujeres reportan significativamente mayor ansiedad. Se recomienda:\n")
    cat("  * Practicar técnicas de relajación antes de dormir\n")
    cat("  * Considerar terapia cognitivo-conductual para el insomnio\n")
    cat("  * Establecer un diario de preocupaciones para liberar estrés antes de dormir\n")
  }
  
  if (resultados[[3]]$P_value < 0.05 && 
      resultados[[3]]$Diferencia_Medias < 0) {
    cat("- Las mujeres duermen significativamente menos entre semana. Se recomienda:\n")
    cat("  * Priorizar el sueño sobre otras actividades\n")
    cat("  * Evitar la procrastinación que lleva a acostarse tarde\n")
    cat("  * Considerar si las responsabilidades están equitativamente distribuidas\n")
  }
  
  # Recomendaciones generales basadas en otros análisis
  cat("\nRECOMENDACIONES GENERALES:\n")
  cat("- Mantener un horario de sueño consistente, incluso los fines de semana\n")
  cat("- Evitar clases muy tempranas cuando sea posible\n")
  cat("- Limitar el uso de dispositivos electrónicos antes de dormir\n")
  cat("- Hacer ejercicio regularmente, pero no cerca de la hora de dormir\n")
  cat("- Crear un ambiente óptimo para dormir (fresco, oscuro y silencioso)\n")
}

# Generar recomendaciones
generar_recomendaciones(resultados_hipotesis)
