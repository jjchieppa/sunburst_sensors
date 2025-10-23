
# CLEAN SUNBURST SENSOR CALIBRATION DATA (SBS2 + SBS3) ####

library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)

# ---- Working directory ----
setwd("C:/Users/JChieppa/Documents/Git/sunburst_sensors/")

# ---- Read the correct Excel file ----
df_raw <- read_excel("Coefficients vs RCO2 at temps_v2.xlsx",
                     sheet = "Raw SAMI Data",
                     col_names = TRUE)

# ---- Inspect the headers ----
print(names(df_raw))


# SBS2 CALIBRATION BLOCKS


sbs2_4.47 <- df_raw %>%
  select(`4.47c`, `...2`, `...3`) %>%
  setNames(c("RCO2", "pCO2", "logpCO2")) %>%
  mutate(Sensor = "SBS2", Temp_C = 4.47)

sbs2_9.71 <- df_raw %>%
  select(`9.71c`, `...6`, `...7`) %>%
  setNames(c("RCO2", "pCO2", "logpCO2")) %>%
  mutate(Sensor = "SBS2", Temp_C = 9.71)

sbs2_14.47 <- df_raw %>%
  select(`14.47c`, `...10`, `...11`) %>%
  setNames(c("RCO2", "pCO2", "logpCO2")) %>%
  mutate(Sensor = "SBS2", Temp_C = 14.47)

sbs2_20.47 <- df_raw %>%
  select(`20.47`, `...14`, `...15`) %>%
  setNames(c("RCO2", "pCO2", "logpCO2")) %>%
  mutate(Sensor = "SBS2", Temp_C = 20.47)

sbs2_25.25 <- df_raw %>%
  select(`25.25c`, `...18`, `...19`) %>%
  setNames(c("RCO2", "pCO2", "logpCO2")) %>%
  mutate(Sensor = "SBS2", Temp_C = 25.25)


# SBS3 CALIBRATION BLOCKS


sbs3_9.73 <- df_raw %>%
  select(`sbs3 9.73C`, `...26`, `...27`) %>%
  setNames(c("RCO2", "pCO2", "logpCO2")) %>%
  mutate(Sensor = "SBS3", Temp_C = 9.73)

sbs3_14.50 <- df_raw %>%
  select(`sbs3 14.50c`, `...30`, `...31`) %>%
  setNames(c("RCO2", "pCO2", "logpCO2")) %>%
  mutate(Sensor = "SBS3", Temp_C = 14.50)

sbs3_20.5 <- df_raw %>%
  select(`SBS3 20.5c`, `...34`, `...35`) %>%
  setNames(c("RCO2", "pCO2", "logpCO2")) %>%
  mutate(Sensor = "SBS3", Temp_C = 20.5)

sbs3_25.30 <- df_raw %>%
  select(`SBS3 25.30c`, `...38`, `...39`) %>%
  setNames(c("RCO2", "pCO2", "logpCO2")) %>%
  mutate(Sensor = "SBS3", Temp_C = 25.30)


# COMBINE AND CLEAN


df_long <- bind_rows(
  sbs2_4.47, sbs2_9.71, sbs2_14.47, sbs2_20.47, sbs2_25.25,
  sbs3_9.73, sbs3_14.50, sbs3_20.5, sbs3_25.30
) %>%
  mutate(across(c(RCO2, pCO2, logpCO2), as.numeric)) %>%
  filter(!is.na(RCO2), !is.na(pCO2), !is.na(logpCO2))


# CHECK RESULTS


df_long %>%
  count(Sensor, Temp_C) %>%
  print(n = Inf)


# filter the data ####
df_long = subset(df_long, logpCO2 <= 3)
df_long = subset(df_long, logpCO2 >= 2.3)

# PLOT TO VISUALLY VERIFY ####

plotly::ggplotly(ggplot(df_long, aes(x = RCO2, y = logpCO2, color = Temp_C)) +
                   geom_point(size = 2) +
                   facet_wrap(~Sensor, scales = "free") +
                   scale_color_viridis_c(option = "plasma") +
                   labs(
                     title = "SAMI Calibration: RCO2 vs log(pCO2)",
                     x = "RCO2", y = "log10(pCO2)", color = "Temperature (°C)"
                   ) +
                   theme_bw() +
                   xlim(0,1.5) +
                   ylim(2,3))