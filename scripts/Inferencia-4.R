# Cargar librerías
library(Lock5Data)
library(tidyverse)
library(ggpubr)

# Cargar datos
data("SleepStudy")
datos <- SleepStudy

# Análisis ANOVA
cat("📊 ANOVA: Consumo de alcohol vs GPA\n")

# Filtrar datos
datos_alcohol <- datos %>% 
  filter(!is.na(AlcoholUse), !is.na(GPA)) %>%
  mutate(AlcoholUse = factor(AlcoholUse, levels = c("Abstain", "Light", "Moderate", "Heavy")))

# Estadísticos descriptivos
cat("\nEstadísticos descriptivos:\n")
datos_alcohol %>%
  group_by(AlcoholUse) %>%
  summarise(n = n(),
            Media = mean(GPA) %>% round(2),
            DE = sd(GPA) %>% round(2)) %>%
  print()

# ANOVA
anova_result <- aov(GPA ~ AlcoholUse, data = datos_alcohol)
summary_anova <- summary(anova_result)
f_val <- summary_anova[[1]]$`F value`[1] %>% round(2)
p_val <- summary_anova[[1]]$`Pr(>F)`[1] %>% round(3)

cat("\nResultados ANOVA:\n")
cat("F =", f_val, "\n")
cat("p =", p_val, ifelse(p_val < 0.05, "✅ (significativo)", "❌ (no significativo)"), "\n")

# Visualización
ggplot(datos_alcohol, aes(x = AlcoholUse, y = GPA, fill = AlcoholUse)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Oranges") +
  labs(title = "GPA por Nivel de Consumo de Alcohol",
       x = "Nivel de Consumo",
       y = "GPA") +
  theme_minimal() +
  theme(legend.position = "none")

# Conclusión
cat("\n🔍 Conclusión:\n")
if(p_val < 0.05){
  cat("Hay diferencias significativas en el GPA entre los niveles de consumo de alcohol.\n")
} else {
  cat("No hay evidencia estadística suficiente para afirmar que el consumo de alcohol\n")
  cat("afecta significativamente el GPA en esta muestra.\n\n")
  cat("🍷 Aunque la tendencia sugiere que los consumidores moderados o heavy tienden a\n")
  cat("tener GPA más bajos, la diferencia no es concluyente.\n")
}
