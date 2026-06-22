library(readxl)
library(ggplot2)
library(scales)
library(dplyr)

#CARGA DEL DATASET
df <- read_excel("rebiun.xlsx")

#CREACION DE UNA COLUMNA CON LAS EMISIONES TOTALES ESTIMADAS DE UN SITIO WEB
df$'Emisiones totales kg' <- df$'Emisiones de CO2' * df$'Visitas al sitio web de la biblioteca' / 1000

total_emisiones_rebiun <- sum(df$'Emisiones totales kg')
total_emisiones_rebiun

#CALCULO DE VALORES ESTADISTICOS GENERALES
summary(df$`Emisiones de CO2`)
sd(df$`Emisiones de CO2`)
summary(df$`emisiones_totales_kg`)
sd(df$`emisiones_totales_kg`)

outliers <- boxplot.stats(df$`Emisiones de CO2`)$out
df[df$`Emisiones de CO2` %in% outliers, ]

View(bibliotecas_sostenibles)

#BOXPLOT - EMISIONES DE CO2 POR VISITA
boxplot_emisiones_co2 <- ggplot(df, aes(x = "", y = `Emisiones de CO2`)) +
  
  geom_boxplot(
    fill = "#001489",
    color = "#001489",
    alpha = 0.7,
    width = 0.3,
    outlier.color = "#001489"
  ) +
  
  geom_hline(
    aes(yintercept = 0.359, linetype = "Mediana global (Website Carbon)"),
    color = "red",
    linewidth = 1
  ) +
  
  geom_point(
    aes(x = "", y = 0.5462432),
    shape = 4,
    size = 4,
    stroke = 1.5,
    color = "#001489"
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    breaks = seq(0, 4, by = 0.5),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  
  scale_linetype_manual(
    name = "",
    values = c("Mediana global (Website Carbon)" = "dashed")
  ) +
  
  labs(
    title = "Distribución de las emisiones de CO2 por visita",
    subtitle = "",
    x = "Bibliotecas",
    y = "Emisiones de CO2 (g por visita)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

boxplot_emisiones_co2
ggsave("boxplot_emisiones_co2.png", plot = boxplot_emisiones_co2, width = 8, height = 5, dpi = 300)

#HISTOGRAMA - CALIFICACION DE SOSTENIBILIDAD
calificacion <- df %>%
  count(`Calificación de sostenibilidad`) %>%
  mutate(porcentaje = n / sum(n))

calificacion$`Calificación de sostenibilidad` <- factor(
  calificacion$`Calificación de sostenibilidad`,
  levels = c("A+", "A", "B", "C", "D", "E", "F")
)

bibliotecas_sostenibles <- df %>% 
  filter((`Calificación de sostenibilidad` %in% "A") | (`Calificación de sostenibilidad`%in% "B")) %>% 
  select('Biblioteca universitaria','Emisiones de CO2', 'Calificación de sostenibilidad')

hist_calificacion_co2 <- ggplot(calificacion, aes(x = `Calificación de sostenibilidad`, y = porcentaje)) +
  
  geom_col(
    fill = "#001489",
    alpha = 0.9,
    width = 0.7
  ) +
  
  geom_text(
    aes(label = percent(porcentaje, accuracy = 0.1)),
    vjust = -0.4,
    size = 4
  ) +
  
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  
  labs(
    title = "Distribución de la calificación de sostenibilidad",
    x = "Calificación de sostenibilidad",
    y = "Bibliotecas (%)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, margin = margin(b = 15), hjust = 0),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

hist_calificacion_co2
ggsave("hist_calificacion_co2.png", plot = hist_calificacion_co2, width = 8, height = 5, dpi = 300)

#BARRAS - TOP 10 UNIVERSIDADES CON MÁS EMISIONES
df_ordenado <- df[order(-df$'Emisiones totales kg'), ]
top10_total <- head(df_ordenado, 10)
top10_total <- top10_total[, c("Biblioteca universitaria", "Visitas al sitio web de la biblioteca", "Emisiones de CO2",  "emisiones_totales_kg")]
View(top10_total)


barras_top10_total <- top10_total %>%
  mutate(`Biblioteca universitaria` = factor(
    `Biblioteca universitaria`,
    levels = rev(`Biblioteca universitaria`)
  ))

suma_top10 <- sum(top10_total$'Emisiones totales kg')
suma_top10_porcentaje <- suma_top10 / total_emisiones_rebiun * 100
suma_top10_porcentaje

bar_top10_total <- ggplot(barras_top10_total, aes(x = `Biblioteca universitaria`, y = `Emisiones totales kg`)) +
  geom_col(
    fill = "#001489",
    width = 0.7
  ) +
  coord_flip() +
  
  labs(
    title = "Top 10 bibliotecas universitarias por emisiones totales de CO2",
    subtitle = "",
    x = "Bibliotecas",
    y = "Emisiones totales de CO2 (kg)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 5, l = -170)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10, l = -170), hjust = 0),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )

bar_top10_total
ggsave("bar_top10_total.png", plot = bar_top10_total, width = 8, height = 5, dpi = 300)

# BARRAS - TOP 10 BIBLIOTECAS CON MÁS VISITAS
df_ordenado_visitas <- df[order(-df$`Visitas al sitio web de la biblioteca`), ]

top10_visitas <- head(df_ordenado_visitas, 10)

top10_visitas <- top10_visitas[, c(
  "Biblioteca universitaria",
  "Visitas al sitio web de la biblioteca",
  "Emisiones de CO2",
  "Emisiones totales kg"
)]

View(top10_visitas)

barras_top10_visitas <- top10_visitas %>%
  mutate(`Biblioteca universitaria` = factor(
    `Biblioteca universitaria`,
    levels = rev(`Biblioteca universitaria`)
  ))

bar_top10_visitas <- ggplot(
  barras_top10_visitas,
  aes(x = `Biblioteca universitaria`, y = `Visitas al sitio web de la biblioteca`)
) +
  geom_col(
    fill = "#001489",
    width = 0.7
  ) +
  coord_flip() +
  
  scale_y_continuous(
    labels = scales::label_number(scale = 1/1000, suffix = "k"),
    breaks = seq(0, 4500000, by = 500000)
  ) +
  
  labs(
    title = "Top 10 bibliotecas universitarias por número de visitas",
    x = "Bibliotecas",
    y = "Visitas al sitio web en miles de visitas (k)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 5, l = -170)),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )

bar_top10_visitas

ggsave("bar_top10_visitas.png", plot = bar_top10_visitas, width = 8, height = 5, dpi = 300)

#BOXPLOT - DIFERENCIA ENTRE TITULARIDAD
df_publicas <- df %>%
  filter(Titularidad == "Pública")

df_privadas <- df %>%
  filter(Titularidad == "Privada")

View(df_publicas)
View(df_privadas)

summary(df_publicas$`Emisiones de CO2`)
sd(df_publicas$`Emisiones de CO2`)

summary(df_privadas$`Emisiones de CO2`)
sd(df_privadas$`Emisiones de CO2`)

boxplot_titularidad <- ggplot(df, aes(x = Titularidad, y = `Emisiones de CO2`)) +
  
  geom_boxplot(
    fill = "#001489",
    color = "#001489",
    alpha = 0.7,
    width = 0.35,
    outlier.color = "#001489"
  ) +
  
  geom_hline(
    aes(yintercept = 0.359, linetype = "Mediana global (Website Carbon)"),
    color = "red",
    linewidth = 1
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    breaks = seq(0, 4, by = 0.5),
    labels = number_format(accuracy = 0.1)
  ) +
  
  scale_linetype_manual(
    name = "",
    values = c("Mediana global (Website Carbon)" = "dashed")
  ) +
  
  labs(
    title = "Distribución de las emisiones de CO2 por titularidad",
    subtitle = "",
    x = "Titularidad",
    y = "Emisiones de CO2 (g por visita)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

boxplot_titularidad
ggsave("boxplot_titularidad.png", plot = boxplot_titularidad, width = 8, height = 5, dpi = 300)
