# ============================================================
# SUNBURST SENSOR CALIBRATION: SBS2, SBS3, & COMBINED
# ============================================================

# LIBRARIES ####
library(dplyr)
library(ggplot2)
library(janitor)
library(viridis)
library(patchwork)

# ============================================================
# 1. SBS2 MODEL ####
# ============================================================

df_sbs2 <- subset(df_long, Sensor == "SBS2")

# ---- Fit 2D polynomial model ----
mod_sbs2 <- lm(logpCO2 ~ poly(RCO2, 2, raw = TRUE) * poly(Temp_C, 2, raw = TRUE),
               data = df_sbs2)

# ---- Predictions ----
df_sbs2$pred_logpCO2 <- predict(mod_sbs2, newdata = df_sbs2)
df_sbs2$resid_log <- df_sbs2$logpCO2 - df_sbs2$pred_logpCO2
df_sbs2$resid_pCO2 <- 10^(df_sbs2$logpCO2) - 10^(df_sbs2$pred_logpCO2)

# ---- 1:1 plot ----
p1_sbs2 <- ggplot(df_sbs2, aes(x = logpCO2, y = pred_logpCO2, color = Temp_C)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 0.8) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "SBS2: Observed vs Predicted (2D Polynomial)",
    x = "Observed log(pCO₂)",
    y = "Predicted log(pCO₂)",
    color = "Temp (°C)"
  ) +
  theme_bw(base_size = 13)

# ---- Residual plot (untransformed) ----
p2_sbs2 <- ggplot(df_sbs2, aes(x = factor(Temp_C), y = resid_pCO2, fill = factor(Temp_C))) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "SBS2: Residuals by Temperature (pCO₂ units)",
    x = "Temperature (°C)",
    y = "Residual (Observed - Predicted pCO₂)"
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

# ---- Combine plots ----
(p1_sbs2 / p2_sbs2) + plot_annotation(title = "SBS2 Calibration Model")

# ============================================================
# 2. SBS3 MODEL ####
# ============================================================

df_sbs3 <- subset(df_long, Sensor == "SBS3")

# ---- Fit 2D polynomial model ----
mod_sbs3 <- lm(logpCO2 ~ poly(RCO2, 2, raw = TRUE) * poly(Temp_C, 2, raw = TRUE),
               data = df_sbs3)

# ---- Predictions ----
df_sbs3$pred_logpCO2 <- predict(mod_sbs3, newdata = df_sbs3)
df_sbs3$resid_log <- df_sbs3$logpCO2 - df_sbs3$pred_logpCO2
df_sbs3$resid_pCO2 <- 10^(df_sbs3$logpCO2) - 10^(df_sbs3$pred_logpCO2)

# ---- 1:1 plot ----
p1_sbs3 <- ggplot(df_sbs3, aes(x = logpCO2, y = pred_logpCO2, color = Temp_C)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 0.8) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "SBS3: Observed vs Predicted (2D Polynomial)",
    x = "Observed log(pCO₂)",
    y = "Predicted log(pCO₂)",
    color = "Temp (°C)"
  ) +
  theme_bw(base_size = 13)

# ---- Residual plot (untransformed) ----
p2_sbs3 <- ggplot(df_sbs3, aes(x = factor(Temp_C), y = resid_pCO2, fill = factor(Temp_C))) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "SBS3: Residuals by Temperature (pCO₂ units)",
    x = "Temperature (°C)",
    y = "Residual (Observed - Predicted pCO₂)"
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

# ---- Combine plots ----
(p1_sbs3 / p2_sbs3) + plot_annotation(title = "SBS3 Calibration Model")

# ============================================================
# 3. UNIVERSAL MODEL (SBS2 + SBS3) ####
# ============================================================

# ---- Fit combined model ----
mod_all <- lm(logpCO2 ~ poly(RCO2, 2, raw = TRUE) * poly(Temp_C, 2, raw = TRUE),
              data = df_long)

# ---- Predictions ----
df_long$pred_logpCO2 <- predict(mod_all, newdata = df_long)
df_long$resid_log <- df_long$logpCO2 - df_long$pred_logpCO2
df_long$resid_pCO2 <- 10^(df_long$logpCO2) - 10^(df_long$pred_logpCO2)

# ---- 1:1 plot ----
p1_all <- ggplot(df_long, aes(x = logpCO2, y = pred_logpCO2, color = Sensor)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 0.8) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Universal Model: Observed vs Predicted (2D Polynomial)",
    x = "Observed log(pCO₂)",
    y = "Predicted log(pCO₂)",
    color = "Sensor"
  ) +
  theme_bw(base_size = 13)

# ---- Residual plot ----
p2_all <- ggplot(df_long, aes(x = factor(Temp_C), y = resid_pCO2, fill = Sensor)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, position = position_dodge(width = 0.8)) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Universal Model: Residuals by Temperature (pCO₂ units)",
    x = "Temperature (°C)",
    y = "Residual (Observed - Predicted pCO₂)"
  ) +
  theme_bw(base_size = 13)

# ---- Combine plots ----
(p1_all / p2_all) + plot_annotation(title = "Universal (SBS2 + SBS3) Model")

ggsave("figures/SBS2_model.png", p1_sbs2 / p2_sbs2, width = 8, height = 10, dpi = 300)
ggsave("figures/SBS3_model.png", p1_sbs3 / p2_sbs3, width = 8, height = 10, dpi = 300)
ggsave("figures/Universal_model.png", p1_all / p2_all, width = 8, height = 10, dpi = 300)
