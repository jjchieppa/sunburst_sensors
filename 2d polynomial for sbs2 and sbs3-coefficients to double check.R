# ============================================================
# EXTRACT AND PRINT 2D POLYNOMIAL COEFFICIENTS
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)

# ---- Helper function to clean and print coefficients ----
extract_coefs <- function(model, label) {
  coefs <- coef(model)
  df <- tibble(term = names(coefs), estimate = as.numeric(coefs))
  cat("\n==============================\n", label, "MODEL COEFFICIENTS\n==============================\n", sep = "")
  print(df, n = Inf)
  return(df)
}

# ---- SBS2 coefficients ----
model_sbs2 <- fits_2d$model[[which(unique(df_long$Sensor) == "SBS2")]]
coefs_sbs2 <- extract_coefs(model_sbs2, "SBS2")

# ---- SBS3 coefficients ----
model_sbs3 <- fits_2d$model[[which(unique(df_long$Sensor) == "SBS3")]]
coefs_sbs3 <- extract_coefs(model_sbs3, "SBS3")

# ---- Combined (both sensors together) ----
model_combined <- lm(logpCO2 ~ poly(RCO2, 2, raw = TRUE) * poly(Temp_C, 2, raw = TRUE), data = df_long)
coefs_combined <- extract_coefs(model_combined, "COMBINED")

# ============================================================
# OPTIONAL: SAVE COEFFICIENTS TO CSV FILES
# ============================================================

dir.create("model_coefficients", showWarnings = FALSE)

# write.csv(coefs_sbs2, "model_coefficients/SBS2_coefficients.csv", row.names = FALSE)
# write.csv(coefs_sbs3, "model_coefficients/SBS3_coefficients.csv", row.names = FALSE)
# write.csv(coefs_combined, "model_coefficients/Combined_coefficients.csv", row.names = FALSE)

cat("\n✅ Coefficient tables saved in model_coefficients/ folder.\n")
