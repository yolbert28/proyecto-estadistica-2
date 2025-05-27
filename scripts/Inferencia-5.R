# Cargar librerías
library(Lock5Data)
library(tidyverse)
library(ggpubr)

# Cargar datos
data("SleepStudy")
datos <- SleepStudy

# Prueba Chi-cuadrado
cat("📊 Asociación entre Género y Cronotipo\n")

# Preparar datos
datos_genero <- datos %>%
  filter(!is.na(Gender), !is.na(LarkOwl)) %>%
  mutate(Gender = factor(Gender, labels = c("Mujer", "Hombre")),
         LarkOwl = factor(LarkOwl, levels = c("Lark", "Neither", "Owl")))

# Tabla de contingencia
tabla_contingencia <- table(datos_genero$Gender, datos_genero$LarkOwl)
cat("\nTabla de contingencia:\n")
print(tabla_contingencia)

# Prueba Chi-cuadrado
chi_test <- chisq.test(tabla_contingencia)
chi_val <- chi_test$statistic %>% round(2)
p_val <- chi_test$p.value %>% round(3)

cat("\nResultados Chi-cuadrado:\n")
cat("χ² =", chi_val, "\n")
cat("p =", p_val, ifelse(p_val < 0.05, "✅", "❌"), "\n")

# Visualización
ggplot(datos_genero, aes(x = Gender, fill = LarkOwl)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribución de Cronotipos por Género",
       x = "Género",
       y = "Proporción",
       fill = "Cronotipo") +
  theme_minimal()

# Conclusión
cat("\n🔍 Conclusión:\n")
if(p_val < 0.05){
  cat("Existe una asociación significativa entre género y cronotipo.\n\n")
  cat("Las mujeres tienden a ser más Lark o Neither\n")
  cat("Los hombres tienen más probabilidad de ser Owl\n\n")
  cat("⚖️ Esto podría explicar diferencias en hábitos de sueño y rendimiento académico entre géneros.\n")
} else {
  cat("No hay evidencia suficiente para afirmar asociación entre género y cronotipo.\n")
}

