# ============================================================
# 2D POLYNOMIAL MODEL: log(pCO2) ~ RCO2 * Temp_C (per sensor)
# ============================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# ---- Fit 2D polynomial per sensor ----
fits_2d <- df_long %>%
  group_by(Sensor) %>%
  do(model = lm(logpCO2 ~ poly(RCO2, 2, raw = TRUE) *
                  poly(Temp_C, 2, raw = TRUE), data = .))

# ---- Make predictions for plotting ----
# ---- Create small prediction grid per sensor ----
grid_2d <- df_long %>%
  group_by(Sensor) %>%
  summarise(
    RCO2_seq = list(seq(min(RCO2), max(RCO2), length.out = 50)),
    Temp_seq = list(seq(min(Temp_C), max(Temp_C), length.out = 50))
  ) %>%
  rowwise() %>%
  mutate(grid = list(expand.grid(RCO2 = RCO2_seq, Temp_C = Temp_seq))) %>%
  dplyr::select(Sensor, grid) %>%
  unnest(grid)


# Predict for each row
grid_2d <- grid_2d %>%
  group_by(Sensor) %>%
  mutate(pred_logpCO2 = predict(fits_2d$model[[which(unique(df_long$Sensor) == unique(Sensor))]],
                                newdata = cur_data()))

# ---- Compare observed vs predicted ----
df_pred <- df_long %>%
  group_by(Sensor) %>%
  mutate(predicted = predict(
    fits_2d$model[[which(unique(df_long$Sensor) == unique(Sensor))]],
    newdata = cur_data()
  ))

# ---- Observed vs Predicted plot ----
ggplot(df_pred, aes(x = logpCO2, y = predicted, color = Temp_C)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 0.8) +
  scale_color_viridis_c(option = "plasma") +
  facet_wrap(~Sensor, scales = "free") +
  labs(
    title = "2D Polynomial Calibration Fit per Sensor",
    x = "Observed log(pCO₂)",
    y = "Predicted log(pCO₂)",
    color = "Temperature (°C)"
  ) +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))
