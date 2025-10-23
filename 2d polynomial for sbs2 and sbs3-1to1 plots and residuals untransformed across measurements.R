# ============================================================
# SUNBURST SENSOR CALIBRATION: RESIDUALS vs pCO₂ (by Temp & Sensor)
# ============================================================

# LIBRARIES ####
library(dplyr)
library(ggplot2)
library(janitor)
library(viridis)
library(patchwork)

# Ensure figures folder exists
dir.create("figures", showWarnings = FALSE)

# ============================================================
# 1. SBS2 MODEL ####
# ============================================================

df_sbs2 <- subset(df_long, Sensor == "SBS2")

# ---- Fit 2D polynomial model ----
mod_sbs2 <- lm(logpCO2 ~ poly(RCO2, 2, raw = TRUE) * poly(Temp_C, 2, raw = TRUE),
               data = df_sbs2)

# ---- Predictions & residuals ----
df_sbs2 <- df_sbs2 %>%
  mutate(
    pred_logpCO2 = predict(mod_sbs2, newdata = df_sbs2),
    resid_log    = logpCO2 - pred_logpCO2,
    obs_pCO2     = 10^logpCO2,
    pred_pCO2    = 10^pred_logpCO2,
    resid_pCO2   = obs_pCO2 - pred_pCO2
  )

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

# ---- Residuals vs pCO₂ plot ----
p2_sbs2 <- ggplot(df_sbs2, aes(x = obs_pCO2, y = resid_pCO2, color = Temp_C)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "SBS2: Residuals vs Observed pCO₂",
    x = "Observed pCO₂",
    y = "Residual (Observed - Predicted pCO₂)",
    color = "Temp (°C)"
  ) +
  theme_bw(base_size = 13)

# ---- Combine plots ----
(p1_sbs2 / p2_sbs2) + plot_annotation(title = "SBS2 Calibration Residual Analysis")

# ============================================================
# 2. SBS3 MODEL ####
# ============================================================

df_sbs3 <- subset(df_long, Sensor == "SBS3")

mod_sbs3 <- lm(logpCO2 ~ poly(RCO2, 2, raw = TRUE) * poly(Temp_C, 2, raw = TRUE),
               data = df_sbs3)

df_sbs3 <- df_sbs3 %>%
  mutate(
    pred_logpCO2 = predict(mod_sbs3, newdata = df_sbs3),
    resid_log    = logpCO2 - pred_logpCO2,
    obs_pCO2     = 10^logpCO2,
    pred_pCO2    = 10^pred_logpCO2,
    resid_pCO2   = obs_pCO2 - pred_pCO2
  )

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

p2_sbs3 <- ggplot(df_sbs3, aes(x = obs_pCO2, y = resid_pCO2, color = Temp_C)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "SBS3: Residuals vs Observed pCO₂",
    x = "Observed pCO₂",
    y = "Residual (Observed - Predicted pCO₂)",
    color = "Temp (°C)"
  ) +
  theme_bw(base_size = 13)

(p1_sbs3 / p2_sbs3) + plot_annotation(title = "SBS3 Calibration Residual Analysis")

# ============================================================
# 3. UNIVERSAL MODEL (SBS2 + SBS3) ####
# ============================================================

mod_all <- lm(logpCO2 ~ poly(RCO2, 2, raw = TRUE) * poly(Temp_C, 2, raw = TRUE),
              data = df_long)

df_long <- df_long %>%
  mutate(
    pred_logpCO2 = predict(mod_all, newdata = df_long),
    resid_log    = logpCO2 - pred_logpCO2,
    obs_pCO2     = 10^logpCO2,
    pred_pCO2    = 10^pred_logpCO2,
    resid_pCO2   = obs_pCO2 - pred_pCO2
  )

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

p2_all <- ggplot(df_long, aes(x = obs_pCO2, y = resid_pCO2, color = Sensor)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Universal Model: Residuals vs Observed pCO₂",
    x = "Observed pCO₂",
    y = "Residual (Observed - Predicted pCO₂)",
    color = "Sensor"
  ) +
  theme_bw(base_size = 13)

(p1_all / p2_all) + plot_annotation(title = "Universal (SBS2 + SBS3) Residual Analysis")

# ============================================================
# SAVE PLOTS ####
# ============================================================
ggsave("figures/SBS2_residuals_vs_pCO2.png", p1_sbs2 / p2_sbs2, width = 8, height = 10, dpi = 300)
ggsave("figures/SBS3_residuals_vs_pCO2.png", p1_sbs3 / p2_sbs3, width = 8, height = 10, dpi = 300)
ggsave("figures/Universal_residuals_vs_pCO2.png", p1_all / p2_all, width = 8, height = 10, dpi = 300)
