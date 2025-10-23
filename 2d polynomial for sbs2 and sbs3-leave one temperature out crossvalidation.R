# ============================================================
# LEAVE-ONE-TEMPERATURE-OUT CROSS VALIDATION (2D POLYNOMIAL)
# ============================================================

library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)

# ---- Define the 2D polynomial model ----
formula_2d <- logpCO2 ~ poly(RCO2, 2, raw = TRUE) * poly(Temp_C, 2, raw = TRUE)

# ---- Unique temperatures ----
temps <- sort(unique(df_long$Temp_C))

# Exclude min and max
temps_to_test <- temps[temps != min(temps) & temps != max(temps)]

# ---- Leave-one-temp-out loop ----
results <- map_dfr(temps_to_test, function(t_out) {
  
  # Split data
  train <- df_long %>% filter(Temp_C != t_out)
  test  <- df_long %>% filter(Temp_C == t_out)
  
  # Fit model
  model <- lm(formula_2d, data = train)
  
  # Predict for left-out temp
  preds <- test %>%
    mutate(
      predicted = predict(model, newdata = test),
      left_out = t_out
    )
  
  preds
})

# ---- Evaluate model performance per left-out temperature ----
cv_summary <- results %>%
  group_by(left_out) %>%
  summarise(
    R2 = cor(predicted, logpCO2)^2,
    RMSE = sqrt(mean((predicted - logpCO2)^2)),
    .groups = "drop"
  )

print(cv_summary)

# ---- Observed vs Predicted plot ----
ggplot(results, aes(x = logpCO2, y = predicted, color = Temp_C)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 0.8) +
  facet_wrap(~ paste0("Left-Out: ", left_out, "°C"), ncol = 3) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Leave-One-Temperature-Out Validation (2D Polynomial Model)",
    x = "Observed log(pCO₂)",
    y = "Predicted log(pCO₂)",
    color = "Actual Temp (°C)"
  ) +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))
