library(GetBCBData)
library(tidyverse)
library(gganimate)
library(gifski)

my.id <- c(exportacoes_eua = 3856)

df.bcb <- gbcbd_get_series(
  id = my.id,
  first.date = "1986-03-06",
  last.date = Sys.Date(),
  format.data = "long",
  use.memoise = TRUE,
  cache.path = tempdir(),
  do.parallel = FALSE
)

df.bcb_clean <- df.bcb %>%
  mutate(ref.date = as.Date(ref.date)) %>%
  arrange(ref.date) %>%
  filter(!is.na(value)) %>%
  distinct(ref.date, .keep_all = TRUE)

max_value <- max(df.bcb_clean$value, na.rm = TRUE)
df.bcb_clean <- df.bcb_clean %>%
  mutate(value_pct = (value / max_value) * 100)

df.bcb_clean <- df.bcb_clean %>%
  mutate(growth = value_pct - lag(value_pct))

top_peaks <- df.bcb_clean %>%
  filter(!is.na(growth)) %>%
  arrange(desc(growth)) %>%
  slice(1:3) %>%
  mutate(
    event = c(
      "Fim Guerra Fria e\nGlobalização acelerada",
      "Boom tecnológico",
      "Recuperação pós-crise\nfinanceira global"
    )
  )

expand_frames <- function(data, peak_date, n) {
  idx <- which(data$ref.date == peak_date)
  if(length(idx) == 0) return(data)
  peak_row <- data[idx, ]
  peak_rows <- peak_row[rep(1, n), ]
  data_expanded <- bind_rows(
    data[1:idx, ],
    peak_rows,
    data[(idx+1):nrow(data), ]
  )
  return(data_expanded)
}

pause_frames <- 15

df_anim <- df.bcb_clean
for (d in top_peaks$ref.date) {
  df_anim <- expand_frames(df_anim, d, pause_frames)
}

last_row <- df_anim[nrow(df_anim), ]
last_rows <- last_row[rep(1, 50), ]
df_anim <- bind_rows(df_anim, last_rows)

label_size <- 3.5

p <- ggplot(df_anim, aes(x = ref.date, y = value_pct)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(data = top_peaks, aes(x = ref.date, y = value_pct),
             color = "blue", size = 3) +
  geom_label(
    data = top_peaks,
    aes(x = ref.date, y = value_pct, label = event),
    size = label_size,
    fill = "white",
    color = "black",
    label.size = NA,
    hjust = 0,
    nudge_x = 30
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = scales::percent_format(scale = 1)
  ) +
  scale_x_date(
    breaks = as.Date(c("1986-01-01", "1990-01-01", "1995-01-01",
                       "2000-01-01", "2005-01-01", "2009-01-01")),
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0.1))  # <== aqui a folga extra no final
  ) +
  labs(
    title = "Exportações EUA",
    subtitle = paste0(min(df.bcb_clean$ref.date), " até ", max(df.bcb_clean$ref.date)),
    x = "",
    y = "Exportações EUA em %"
  ) +
  theme_light() +
  transition_reveal(ref.date)

anim <- animate(
  p,
  renderer = gifski_renderer(),
  width = 900,    # um pouco mais largo no output final também
  height = 600,
  duration = 25,
  fps = 20
)

anim_save("grafico_exportacoes_eua_com_eventos_pausa_final_largura.gif", animation = anim)










