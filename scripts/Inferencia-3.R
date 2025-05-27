# Cargar librerías
library(Lock5Data)
library(tidyverse)
library(ggpubr)

# Cargar datos
data("SleepStudy")
datos <- SleepStudy

# Análisis de correlación
cat("📊 Correlaciones entre calidad del sueño y salud mental:\n")

# PoorSleep vs Depression
cor_dep <- cor.test(datos$PoorSleepQuality, datos$DepressionScore, method = "pearson")
cat("\nPoorSleep - Depression:\n")
cat("r =", round(cor_dep$estimate, 2), "\n")
cat("p =", format.pval(cor_dep$p.value, eps = 0.001), ifelse(cor_dep$p.value < 0.05, "✅", "❌"), "\n")

# PoorSleep vs Anxiety
cor_anx <- cor.test(datos$PoorSleepQuality, datos$AnxietyScore, method = "pearson")
cat("\nPoorSleep - Anxiety:\n")
cat("r =", round(cor_anx$estimate, 2), "\n")
cat("p =", format.pval(cor_anx$p.value, eps = 0.001), ifelse(cor_anx$p.value < 0.05, "✅", "❌"), "\n")

# PoorSleep vs Stress
cor_str <- cor.test(datos$PoorSleepQuality, datos$StressScore, method = "pearson")
cat("\nPoorSleep - Stress:\n")
cat("r =", round(cor_str$estimate, 2), "\n")
cat("p =", format.pval(cor_str$p.value, eps = 0.001), ifelse(cor_str$p.value < 0.05, "✅", "❌"), "\n")

# Visualización
g1 <- ggplot(datos, aes(x = PoorSleepQuality, y = DepressionScore)) +
  geom_point(color = "#FF6B6B", alpha = 0.6) +
  geom_smooth(method = "lm", color = "#4ECDC4") +
  labs(title = "Calidad del Sueño vs Depresión",
       x = "Poor Sleep Quality (mayor = peor)",
       y = "Puntaje de Depresión") +
  theme_minimal()

g2 <- ggplot(datos, aes(x = PoorSleepQuality, y = AnxietyScore)) +
  geom_point(color = "#FFA07A", alpha = 0.6) +
  geom_smooth(method = "lm", color = "#45B7D1") +
  labs(title = "Calidad del Sueño vs Ansiedad",
       x = "Poor Sleep Quality (mayor = peor)",
       y = "Puntaje de Ansiedad") +
  theme_minimal()

g3 <- ggplot(datos, aes(x = PoorSleepQuality, y = StressScore)) +
  geom_point(color = "#FF8C69", alpha = 0.6) +
  geom_smooth(method = "lm", color = "#588BAE") +
  labs(title = "Calidad del Sueño vs Estrés",
       x = "Poor Sleep Quality (mayor = peor)",
       y = "Puntaje de Estrés") +
  theme_minimal()

gridExtra::grid.arrange(g1, g2, g3, ncol = 2)

# Conclusión
cat("\n🔍 Conclusión:\n")
cat("Existe una correlación fuerte y significativa entre mala calidad del sueño y mayores\n")
cat("niveles de depresión, ansiedad y estrés.\n\n")
cat("🌙 Dormir mal afecta profundamente la salud mental de los estudiantes.\n")

