# Cargar librerías necesarias
library(Lock5Data)
library(tidyverse)
library(ggpubr)
library(rstatix)

# Cargar y preparar datos
data("SleepStudy")
datos <- SleepStudy %>%
  filter(!is.na(EarlyClass), !is.na(GPA)) %>%
  mutate(EarlyClass = factor(EarlyClass, labels = c("No", "Sí")))

## 1. Estadísticos Descriptivos
cat("📊 Estadísticos Descriptivos:\n")
estadisticos <- datos %>%
  group_by(EarlyClass) %>%
  summarise(
    n = n(),
    Media = mean(GPA) %>% round(3),
    DE = sd(GPA) %>% round(3),
    Min = min(GPA) %>% round(2),
    Max = max(GPA) %>% round(2),
    .groups = 'drop'
  )
print(estadisticos)

## 2. Visualización de Datos
g_boxplot <- ggplot(datos, aes(x = EarlyClass, y = GPA, fill = EarlyClass)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62")) +
  labs(title = "Distribución de GPA por Horario de Clases",
       subtitle = "Comparación entre estudiantes con y sin clases antes de las 9:00 a.m.",
       x = "Tiene clases antes de las 9:00 a.m.",
       y = "GPA") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(g_boxplot)

## 3. Prueba de Supuestos

# Normalidad por grupo
cat("\n🔍 Prueba de Normalidad (Shapiro-Wilk):\n")
normalidad <- datos %>%
  group_by(EarlyClass) %>%
  summarise(
    Estadistico_W = shapiro.test(GPA)$statistic,
    p_valor = shapiro.test(GPA)$p.value
  )
print(normalidad)

# Homogeneidad de varianzas (Levene)
cat("\n🔍 Prueba de Homogeneidad de Varianzas (Levene):\n")
levene_test(GPA ~ EarlyClass, data = datos) %>% print()

## 4. Prueba t de Student
cat("\n📊 Resultados de la prueba t:\n")
t_result <- t.test(GPA ~ EarlyClass, data = datos, var.equal = TRUE)
print(t_result)

# Extraer valores clave
t_val <- t_result$statistic %>% round(3)
p_val <- t_result$p.value %>% round(3)
dif_medias <- t_result$estimate[1] - t_result$estimate[2] %>% round(3)

## 5. Reporte de Resultados Estructurado
cat("\n\n📌 RESULTADOS COMPLETOS:\n")
cat("Hipótesis:\n")
cat("H₀: No hay diferencia en el GPA según si tiene clases antes de las 9:00 a.m.\n")
cat("H₁: Sí hay diferencia.\n\n")

cat("📊 Prueba utilizada: t de Student para muestras independientes\n\n")

cat("📈 Resultados:\n")
cat("t =", t_val, "\n")
cat("p =", p_val, ifelse(p_val < 0.05, "✅ (significativo)", "❌ (no significativo)"), "\n")
cat("Diferencia de medias =", dif_medias, "\n\n")

if(p_val < 0.05){
  cat("🔍 Conclusión:\n")
  if(t_val < 0){
    cat("Los estudiantes CON clases tempranas tienen un GPA significativamente MENOR (", 
        estadisticos$Media[estadisticos$EarlyClass == "Sí"], ") que quienes NO las tienen (", 
        estadisticos$Media[estadisticos$EarlyClass == "No"], ").\n", sep = "")
  } else {
    cat("Los estudiantes CON clases tempranas tienen un GPA significativamente MAYOR (", 
        estadisticos$Media[estadisticos$EarlyClass == "Sí"], ") que quienes NO las tienen (", 
        estadisticos$Media[estadisticos$EarlyClass == "No"], ").\n", sep = "")
  }
  
  cat("\n💡 Esto respalda investigaciones que sugieren que comenzar clases muy temprano\n")
  cat("puede ser perjudicial para el rendimiento académico.\n")
} else {
  cat("🔍 Conclusión:\n")
  cat("No hay evidencia suficiente para afirmar diferencias significativas\n")
  cat("en el GPA entre estudiantes con y sin clases antes de las 9:00 a.m.\n")
}

## 6. Gráfico de densidad comparativo
g_density <- ggplot(datos, aes(x = GPA, fill = EarlyClass)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"), 
                    name = "Clases antes\nde 9:00 a.m.") +
  labs(title = "Distribución Comparativa de GPA",
       subtitle = "Densidad por condición de horario",
       x = "GPA",
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(g_density)