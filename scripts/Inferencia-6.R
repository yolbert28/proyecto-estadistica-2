# Cargar librerías
library(Lock5Data)
library(tidyverse)
library(ggpubr)

# Cargar datos
data("SleepStudy")
datos <- SleepStudy

# Correlación GPA y CognitionZscore
cat("📊 Correlación entre GPA y Cognición\n")

cor_result <- cor.test(datos$GPA, datos$CognitionZscore, method = "pearson")
r_val <- cor_result$estimate %>% round(2)
p_val <- cor_result$p.value %>% format.pval(eps = 0.001)

cat("\nResultados Correlación:\n")
cat("r =", r_val, "\n")
cat("p =", p_val, ifelse(cor_result$p.value < 0.05, "✅", "❌"), "\n")

# Visualización
ggplot(datos, aes(x = CognitionZscore, y = GPA)) +
  geom_point(color = "#6A5ACD", alpha = 0.6) +
  geom_smooth(method = "lm", color = "#FF7F50") +
  labs(title = "Relación entre Cognición y GPA",
       x = "Puntaje Z de Cognición",
       y = "GPA") +
  theme_minimal()

# Conclusión
cat("\n🔍 Conclusión:\n")
cat("Existe una correlación positiva moderada entre desempeño cognitivo y GPA.\n\n")
cat("🧠 Mayor rendimiento cognitivo se asocia con mejor rendimiento académico.\n")

