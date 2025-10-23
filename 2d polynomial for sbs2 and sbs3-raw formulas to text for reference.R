# ============================================================
# WRITE OUT 2D POLYNOMIAL FORMULAS (SBS2 / SBS3 / COMBINED)
# directly from model coefficients (index-based extraction)
# ============================================================

library(dplyr)

# ---- 1) Pull models from fits_2d ----
model_sbs2 <- fits_2d$model[[which(unique(df_long$Sensor) == "SBS2")]]
model_sbs3 <- fits_2d$model[[which(unique(df_long$Sensor) == "SBS3")]]
model_combined <- lm(
  logpCO2 ~ poly(RCO2, 2, raw = TRUE) * poly(Temp_C, 2, raw = TRUE),
  data = df_long
)

# ---- 2) Helper: extract coefficients into 9-index vector ----
coefs_to_a <- function(mod) {
  cf <- coef(mod)
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

a_sbs2     <- coefs_to_a(model_sbs2)
a_sbs3     <- coefs_to_a(model_sbs3)
a_combined <- coefs_to_a(model_combined)

# ---- 3) Helper: formatted polynomial string using indexing ----
make_formula <- function(label, a) {
  fp <- function(x) sprintf("%.12f", x)
  sprintf(
    "==============================
%s MODEL
==============================
log10(pCO2) = %s + %s*R + %s*T + %s*R^2 + %s*R*T + %s*T^2 + %s*R^2*T + %s*R*T^2 + %s*R^2*T^2
pCO2 = 10^(that)

",
    label,
    fp(a[1]), fp(a[2]), fp(a[4]), fp(a[3]), fp(a[6]),
    fp(a[5]), fp(a[7]), fp(a[8]), fp(a[9])
  )
}

# ---- 4) Build + write to file ----
dir.create("formulas", showWarnings = FALSE)
outfile <- "formulas/sunburst_polynomial_formulas.txt"

text_content <- paste0(
  make_formula("SBS2", a_sbs2),
  make_formula("SBS3", a_sbs3),
  make_formula("Combined", a_combined)
)

writeLines(text_content, outfile)
message("✅ Formulas written to: ", outfile)
