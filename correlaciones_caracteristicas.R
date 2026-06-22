library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(corrplot)
library(patchwork)

#CARGA DEL DATASET
df <- read_excel("rebiun.xlsx")

#CREACION DE UNA COLUMNA CON LAS EMISIONES TOTALES ESTIMADAS DE UN SITIO WEB
df$'Emisiones totales kg' <- df$'Emisiones de CO2' * df$'Visitas al sitio web de la biblioteca' / 1000
total_emisiones_rebiun <- sum(df$emisiones_totales_kg)

#VARIABLES A CORRELACIONAR
variables_institucionales <- c(
  "Total de usuarios potenciales",
  "Número de estudiantes",
  "Número de docentes",
  "Personal técnico de gestión y administración",
  "Personal empleado investigador",
  "Plantilla total de la biblioteca",
  "Personal bibliotecario",
  "Personal auxiliar de biblioteca",
  "Estudiantado trabajando con beca",
  "Personal especializado",
  "Personal administrativo",
  "Gasto en recursos de información",
  "Gasto en información electrónica",
  "Coste total del personal",
  "Coste total del personal especializado"
)

#FUNCIÓN PARA EL CALCULO DE CORRELACIONES
calcular_correlaciones <- function(df, variable_objetivo, variables_explicativas) {
  
  vars <- df %>%
    select(all_of(c(variable_objetivo, variables_explicativas))) %>%
    mutate(across(everything(), as.numeric))
  
  correlaciones <- sapply(vars[-1], function(x) {
    cor(vars[[variable_objetivo]], x, use = "complete.obs")
  })
  
  p_values <- sapply(vars[-1], function(x) {
    cor.test(vars[[variable_objetivo]], x)$p.value
  })
  
  tabla <- data.frame(
    Variable = names(correlaciones),
    Correlacion = round(correlaciones, 3),
    `p-value` = round(p_values, 3)
  ) %>%
    arrange(desc(abs(Correlacion)))
  
  return(tabla)
}

#TABLA DE CORRELACIONES CON RESPECTO A LA VARIABLE EMISIONES POR VISITA
tabla_cor_emisiones_visita <- calcular_correlaciones(
  df = df,
  variable_objetivo = "Emisiones de CO2",
  variables_explicativas = variables_institucionales
)

tabla_cor_emisiones_visita

#CORRELACION EMISIONES POR VISITA - COSTE PERSONAL ESPECIALIZADO
coste_especializado_emisiones <- ggplot(df, aes(x = `Coste total del personal especializado`, y = `Emisiones de CO2`)) +
  geom_point(color = "#001489", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  
  labs(
    title = "Relación entre el coste de personal especializado y\nlas emisiones de CO2",
    subtitle = "",
    x = "Coste total del personal especializado (€)",
    y = "Emisiones de CO2 (g por visita)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, margin = margin(b = 15), hjust = 0),
    axis.title = element_text(face = "bold")
  )

coste_especializado_emisiones
ggsave("coste_especializado-emisiones.png", plot = coste_especializado_emisiones, width = 6, height = 6, dpi = 300)

#CORRELACION EMISIONES POR VISITA - PESO
peso_emisiones <- ggplot(df, aes(x = `Peso total de la página`, y = `Emisiones de CO2`)) +
  geom_point(color = "#001489", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  
  labs(
    title = "Relación entre el peso total de la página y las\nemisiones de CO2",
    subtitle = "",
    x = "Peso total de la página (MB)",
    y = "Emisiones de CO2 (g por visita)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, margin = margin(b = 15), hjust = 0),
    axis.title = element_text(face = "bold")
  )

ggsave("peso-emisiones.png", plot = peso_emisiones, width = 6, height = 6, dpi = 300)


#CORRELACION EMISIONES POR VISITA - RENDIMIENTO
cor.test(
  df$`Rendimiento en dispositivos móviles`,
  df$`Emisiones de CO2`,
  use = "complete.obs",
  method = "pearson"
)

cor.test(
  df$`Rendimiento en escritorio`,
  df$`Emisiones de CO2`,
  use = "complete.obs",
  method = "pearson"
)
#RENDIMIENDO EN MOVIL
g_movil <- ggplot(df, aes(x = `Rendimiento en dispositivos móviles`, y = `Emisiones de CO2`)) +
  geom_point(color = "#001489", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Rendimiento en dispositivos móviles",
    y = "Emisiones de CO2 (g por visita)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

#RENDIMIENTO EN ESCRITORIO
g_escritorio <- ggplot(df, aes(x = `Rendimiento en escritorio`, y = `Emisiones de CO2`)) +
  geom_point(color = "#001489", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Rendimiento en escritorio",
    y = "Emisiones de CO2 (g por visita)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# GRID AMBOS GRAFICOS
grid_rendimiento <- g_movil + g_escritorio +
  plot_annotation(
    title = "Relación entre rendimiento web y emisiones de CO2",
    subtitle = "",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, margin = margin(b = 15))
    )
  )

grid_rendimiento

ggsave("grid-rendimiento.png", plot = grid_rendimiento, width = 8, height = 5, dpi = 300)
