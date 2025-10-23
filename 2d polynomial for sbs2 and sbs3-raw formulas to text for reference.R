# ============================================================
# WRITE OUT 2D POLYNOMIAL FORMULAS FOR SBS2 / SBS3 / COMBINED
# ============================================================

# ---- Setup ----
dir.create("formulas", showWarnings = FALSE)

outfile <- "formulas/sunburst_polynomial_formulas.txt"

# ---- Coefficients ----
coefs_sbs2 <- c(
  a00 = 1.7483570601,
  a10 = 0.7579895184,
  a01 = -0.0025557667,
  a20 = 0.1399634414,
  a11 = 0.0453932937,
  a02 = 0.0004011689,
  a21 = -0.0236484177,
  a12 = -0.0012093579,
  a22 = 0.0006531613
)

coefs_sbs3 <- c(
  a00 = 1.8123,
  a10 = 0.7321,
  a01 = -0.0028,
  a20 = 0.1425,
  a11 = 0.0469,
  a02 = 0.00039,
  a21 = -0.024,
  a12 = -0.0011,
  a22 = 0.00065
)

coefs_combined <- c(
  a00 = 1.7804,
  a10 = 0.7498,
  a01 = -0.0026,
  a20 = 0.1412,
  a11 = 0.0457,
  a02 = 0.00040,
  a21 = -0.0239,
  a12 = -0.00120,
  a22 = 0.00065
)

# ---- Helper function to format polynomial text ----
make_formula <- function(label, coefs) {
  sprintf(
    "==============================\n%s MODEL\n==============================\nlog10(pCO2) = %.10f + %.10f*R + %.10f*T + %.10f*R^2 + %.10f*R*T + %.10f*T^2 + %.10f*R^2*T + %.10f*R*T^2 + %.10f*R^2*T^2\npCO2 = 10^(that)\n\n",
    label,
    coefs["a00"], coefs["a10"], coefs["a01"], coefs["a20"], coefs["a11"],
    coefs["a02"], coefs["a21"], coefs["a12"], coefs["a22"]
  )
}

# ---- Build content ----
text_content <- paste0(
  make_formula("SBS2", coefs_sbs2),
  make_formula("SBS3", coefs_sbs3),
  make_formula("Combined", coefs_combined)
)

# ---- Write to text file ----
writeLines(text_content, outfile)

message("✅ Formulas written to: ", outfile)
