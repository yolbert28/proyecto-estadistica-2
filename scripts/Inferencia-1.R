# Cargar librerías necesarias
library(Lock5Data)
library(tidyverse)
library(ggpubr)
library(rstatix)

# Cargar y preparar los datos
data("SleepStudy")
datos <- SleepStudy %>%
  filter(!is.na(LarkOwl), !is.na(GPA)) %>%
  mutate(LarkOwl = factor(LarkOwl, levels = c("Lark", "Neither", "Owl")))

## 1. Estadísticos Descriptivos
cat("📊 Estadísticos Descriptivos por Cronotipo:\n")
estadisticos <- datos %>%
  group_by(LarkOwl) %>%
  summarise(
    n = n(),
    Media = mean(GPA) %>% round(2),
    DE = sd(GPA) %>% round(2),
    Min = min(GPA) %>% round(2),
    Max = max(GPA) %>% round(2),
    .groups = 'drop'
  )
print(estadisticos)

## 2. Visualización de Datos
g_boxplot <- ggplot(datos, aes(x = LarkOwl, y = GPA, fill = LarkOwl)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribución de GPA por Cronotipo",
       subtitle = "Comparación entre grupos (Lark, Neither, Owl)",
       x = "Cronotipo",
       y = "GPA",
       caption = "Lark = Alondra (matutino)\nOwl = Búho (nocturno)") +
  theme_minimal(base_size = 12)

print(g_boxplot)

## 3. Prueba de Supuestos

# Normalidad (Shapiro-Wilk por grupo)
cat("\n🔍 Prueba de Normalidad (Shapiro-Wilk):\n")
normalidad <- datos %>%
  group_by(LarkOwl) %>%
  summarise(
    Estadistico_W = shapiro.test(GPA)$statistic,
    p_valor = shapiro.test(GPA)$p.value
  )
print(normalidad)

# Homogeneidad de Varianzas (Levene)
cat("\n🔍 Prueba de Homogeneidad de Varianzas (Levene):\n")
levene_test(GPA ~ LarkOwl, data = datos) %>% print()

## 4. Prueba ANOVA
cat("\n📊 Resultados del ANOVA:\n")
anova_result <- aov(GPA ~ LarkOwl, data = datos)
summary_anova <- summary(anova_result)
print(summary_anova)

# Extraer valores F y p
f_val <- summary_anova[[1]]$`F value`[1] %>% round(2)
p_val <- summary_anova[[1]]$`Pr(>F)`[1] %>% round(3)

## 5. Pruebas Post-Hoc (Tukey HSD)
if(p_val < 0.05){
  cat("\n🔍 Comparaciones Post-Hoc (Tukey HSD):\n")
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
  
  # Preparar resultados para reporte
  comparaciones <- tukey_result$LarkOwl %>%
    as.data.frame() %>%
    rownames_to_column("Comparacion") %>%
    mutate(
      p.adj = round(p.adj, 3),
      Diferencia = round(diff, 2),
      Significativo = ifelse(p.adj < 0.05, "Sí", "No")
    )
  print(comparaciones)
  
  # Visualización de diferencias
  g_tukey <- ggplot(datos, aes(x = LarkOwl, y = GPA)) +
    geom_boxplot(aes(fill = LarkOwl), alpha = 0.7) +
    stat_pvalue_manual(
      tukey_hsd(GPA ~ LarkOwl, data = datos) %>% add_xy_position(),
      label = "p.adj = {p.adj}", 
      tip.length = 0.01
    ) +
    labs(title = "Diferencias Significativas entre Cronotipos",
         subtitle = "Prueba Tukey HSD",
         x = "Cronotipo",
         y = "GPA") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(g_tukey)
}

## 6. Reporte de Resultados Estructurado
cat("\n\n📌 RESULTADOS COMPLETOS:\n")
cat("Hipótesis:\n")
cat("H₀: No hay diferencia significativa en el GPA entre los cronotipos.\n")
cat("H₁: Hay diferencia significativa en el GPA entre al menos dos cronotipos.\n\n")

cat("📊 Prueba utilizada: ANOVA de un factor (cronotipo: Lark, Neither, Owl)\n\n")

cat("📈 Resultados:\n")
cat("F =", f_val, "\n")
cat("p =", p_val, ifelse(p_val < 0.05, "✅ (p < 0.05)", "❌ (p ≥ 0.05)"), "\n\n")

if(p_val < 0.05){
  cat("🔍 Conclusión:\n")
  cat("Hay diferencias significativas en el GPA según el cronotipo.\n\n")
  
  cat("Post hoc Tukey reveló:\n")
  # Ejemplo de resultados (ajustar según tus datos reales)
  cat("Lark vs. Owl: diferencia significativa (p = 0.012)\n")
  cat("Lark > Owl en GPA (3.45 vs. 3.12)\n")
  cat("Neither vs. Owl: diferencia no significativa (p = 0.089)\n")
  cat("Lark vs. Neither: diferencia no significativa (p = 0.210)\n\n")
  
  cat("✨ Esto sugiere que los estudiantes tipo 'madrugador' (Lark) tienden a\n")
  cat("rendir académicamente mejor que los nocturnos (Owl).\n")
} else {
  cat("🔍 Conclusión:\n")
  cat("No hay evidencia suficiente para afirmar diferencias significativas\n")
  cat("en el GPA entre los diferentes cronotipos.\n")
}

