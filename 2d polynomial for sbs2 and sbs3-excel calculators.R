# ============================================================
# REBUILD EXCEL CALCULATORS FROM ACTUAL FITTED COEFFICIENTS
# (High precision, embedded formula at T7/Z7 -> AD7)
# ============================================================

library(dplyr)
library(openxlsx)

# ---- 1) Pull models ----
model_sbs2 <- fits_2d$model[[which(unique(df_long$Sensor) == "SBS2")]]
model_sbs3 <- fits_2d$model[[which(unique(df_long$Sensor) == "SBS3")]]
model_combined <- lm(logpCO2 ~ poly(RCO2, 2, raw = TRUE) * poly(Temp_C, 2, raw = TRUE),
                     data = df_long)

# ---- 2) Convert raw poly coefs -> named a_ij (uncentered 2D polynomial) ----
# Model terms are: (Intercept), R, R^2, T, T^2, R*T, R^2*T, R*T^2, R^2*T^2
coefs_to_a <- function(mod) {
  cf <- coef(mod)
  # helper to safely grab by name (falls back to 0 if missing)
  g <- function(nm) if (nm %in% names(cf)) cf[[nm]] else 0
  
  c(
    a00 = g("(Intercept)"),
    a10 = g("poly(RCO2, 2, raw = TRUE)1"),
    a20 = g("poly(RCO2, 2, raw = TRUE)2"),
    a01 = g("poly(Temp_C, 2, raw = TRUE)1"),
    a02 = g("poly(Temp_C, 2, raw = TRUE)2"),
    a11 = g("poly(RCO2, 2, raw = TRUE)1:poly(Temp_C, 2, raw = TRUE)1"),
    a21 = g("poly(RCO2, 2, raw = TRUE)2:poly(Temp_C, 2, raw = TRUE)1"),
    a12 = g("poly(RCO2, 2, raw = TRUE)1:poly(Temp_C, 2, raw = TRUE)2"),
    a22 = g("poly(RCO2, 2, raw = TRUE)2:poly(Temp_C, 2, raw = TRUE)2")
  )
}

a_sbs2      <- coefs_to_a(model_sbs2)
a_sbs3      <- coefs_to_a(model_sbs3)
a_combined  <- coefs_to_a(model_combined)

# ---- 3) Excel builder (T7 = Temp_C, Z7 = RCO2, AD7 = Pred pCO2) ----
make_calculator <- function(sensor_label, a, outfile) {
  wb <- createWorkbook()
  addWorksheet(wb, "Calculator")
  
  # headers at row 6
  writeData(wb, "Calculator", "Temp_C (°C)", startCol = 20, startRow = 6)  # T6
  writeData(wb, "Calculator", "RCO2",        startCol = 26, startRow = 6)  # Z6
  writeData(wb, "Calculator", "Pred_pCO2",   startCol = 30, startRow = 6)  # AD6
  
  # seed example inputs at row 7 (you can change these)
  writeData(wb, "Calculator", 15,   startCol = 20, startRow = 7)  # T7 (Temp_C)
  writeData(wb, "Calculator", 0.85, startCol = 26, startRow = 7)  # Z7 (RCO2)
  
  # embed formula with high precision (12 digits)
  fp <- function(x) sprintf("%.12f", x)  # 12 significant digits
  
  log_expr <- paste0(
    "(",
    fp(a["a00"]), " + ",
    fp(a["a10"]), "*Z7 + ",
    fp(a["a01"]), "*T7 + ",
    fp(a["a20"]), "*Z7^2 + ",
    fp(a["a11"]), "*Z7*T7 + ",
    fp(a["a02"]), "*T7^2 + ",
    fp(a["a21"]), "*Z7^2*T7 + ",
    fp(a["a12"]), "*Z7*T7^2 + ",
    fp(a["a22"]), "*Z7^2*T7^2",
    ")"
  )
  
  # Predicted pCO2 on AD7
  writeFormula(wb, "Calculator", x = paste0("=10^", log_expr), startCol = 30, startRow = 7)
  
  # cosmetic
  addStyle(wb, "Calculator", createStyle(textDecoration = "bold"),
           rows = 6, cols = c(20, 26, 30), gridExpand = TRUE)
  setColWidths(wb, "Calculator", cols = c(20, 26, 30), widths = 15)
  
  # little note with the full log10 expression (for QA)
  writeData(wb, "Calculator", "log10(pCO2) = ", startCol = 20, startRow = 9)
  writeData(wb, "Calculator", gsub("\\+", "+ ", log_expr), startCol = 21, startRow = 9)
  
  saveWorkbook(wb, outfile, overwrite = TRUE)
  message("✅ Created: ", outfile)
}

# ---- 4) Write files ----
dir.create("excel_calculators", showWarnings = FALSE)
make_calculator("SBS2",     a_sbs2,     "excel_calculators/SBS2_Calculator.xlsx")
make_calculator("SBS3",     a_sbs3,     "excel_calculators/SBS3_Calculator.xlsx")
make_calculator("Combined", a_combined, "excel_calculators/Combined_Calculator.xlsx")

message("\nAll calculators rebuilt with embedded formulas @ T7/Z7 → AD7 (12-digit precision).")
