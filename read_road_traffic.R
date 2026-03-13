library(readxl)
library(dplyr)
library(purrr)
library(lubridate)
library(synthdid)

# Directory containing the Excel files
traffic_dir <- "data/raw_data/road_traffic"

# List all xlsx files (exclude any subdirectories like "aggregate")
traffic_files <- list.files(traffic_dir, pattern = "^\\d{4}_\\d{2}\\.xlsx$", full.names = TRUE)

# Function to read one file and extract the relevant columns
read_traffic_file <- function(filepath) {
  # Extract year and month from filename (e.g. "2021_01.xlsx" -> year=2021, month=1)
  filename <- tools::file_path_sans_ext(basename(filepath))
  parts    <- strsplit(filename, "_")[[1]]
  year_val <- as.integer(parts[1])
  month_val <- as.integer(parts[2])

  # Read the "Monthly average" sheet, skipping the first 6 rows so that
  # row 7 of the sheet becomes the column header row.
  # Column layout (1-indexed):
  #   1  = Nr.
  #   2  = Measuring station
  #   3  = (empty / merged cell)
  #   4  = Ct
  #   5  = Road
  #   6  = Monthly avg – Working days Tu-Th
  #   7  = Monthly avg – Working days Mo-Fr
  #   8  = Monthly avg – Saturdays
  #   9  = Monthly avg – Sundays
  #  10  = Monthly avg – Every day of the week Mo-Su  <-- column we want
  df <- read_excel(
    path      = filepath,
    sheet     = "Monthly average",
    skip      = 6,          # skip metadata rows 1-6; row 7 becomes header
    col_names = TRUE
  )

  # Column 10 has no label in row 7 (the label "Every day of the week Mo-Su"
  # is one row above, in row 6). readxl names it with a generic placeholder;
  # select by position to be robust across file versions.
  df_subset <- df[, c(1, 2, 4, 5, 10)]
  colnames(df_subset) <- c("nr", "measuring_station", "ct", "road", "traffic_mo_su")

  # Drop any rows where Nr. is missing (footer or empty rows)
  df_subset <- df_subset[!is.na(df_subset$nr), ]

  # Add month identifier as a Date (first day of the month)
  df_subset$month <- as.Date(paste(year_val, month_val, "01", sep = "-"))

  df_subset
}

# Read and combine all files
road_traffic <- map_dfr(traffic_files, read_traffic_file)

# Ensure sensible column types
road_traffic <- road_traffic %>%
  mutate(
    nr                = as.character(nr),
    measuring_station = as.character(measuring_station),
    ct                = as.character(ct),
    road              = as.character(road),
    traffic_mo_su     = as.numeric(traffic_mo_su)
  ) %>%
  arrange(month, nr)

glimpse(road_traffic)


#### Synthetic DiD ####

# Treatment definition:
#   Treated units : all measuring stations in Zurich (ZH) or Fribourg (FR)
#   Control units : all other stations
#   Treatment date: September 2022

treat_date <- as.Date("2022-09-01")

road_traffic_sdid <- road_traffic %>%
  mutate(
    treated_unit = ct %in% c("ZH", "FR"),
    treatment    = as.integer(treated_unit & month >= treat_date)
  )

# ── Balance the panel ────────────────────────────────────────────────────────
# synthdid requires a balanced panel (every unit present in every period).
# Keep only stations observed in ALL time periods and with no missing outcome.

total_months <- n_distinct(road_traffic_sdid$month)

balanced_nrs <- road_traffic_sdid %>%
  filter(!is.na(traffic_mo_su)) %>%
  group_by(nr) %>%
  summarise(n_months = n_distinct(month), .groups = "drop") %>%
  filter(n_months == total_months) %>%
  pull(nr)

panel_long <- road_traffic_sdid %>%
  filter(nr %in% balanced_nrs) %>%
  # Use integer time label YYYYMM so periods are clearly ordered and labelled
  mutate(time = as.integer(format(month, "%Y%m"))) %>%
  select(unit = nr, time, outcome = traffic_mo_su, treatment) %>%
  arrange(unit, time)

cat(sprintf(
  "Balanced panel: %d stations × %d months (%d treated, %d control)\n",
  n_distinct(panel_long$unit),
  n_distinct(panel_long$time),
  n_distinct(panel_long$unit[panel_long$treatment == 1]),
  n_distinct(panel_long$unit[panel_long$treatment == 0])
))

# ── Prepare matrices for synthdid ────────────────────────────────────────────
# panel.matrices() converts the long data frame into the Y outcome matrix,
# and returns N0 (# control units) and T0 (# pre-treatment periods) needed
# by synthdid_estimate().

setup <- panel.matrices(panel_long)

# ── Estimation ───────────────────────────────────────────────────────────────
tau_hat <- synthdid_estimate(setup$Y, setup$N0, setup$T0)

# Standard error via placebo variance method
se <- sqrt(vcov(tau_hat, method = "placebo"))

cat(sprintf("Synthetic DiD estimate : %+.1f vehicles/day\n",   as.numeric(tau_hat)))
cat(sprintf("Std. error             :  %.1f\n",                 se))
cat(sprintf("95%% CI                : (%+.1f, %+.1f)\n",
            as.numeric(tau_hat) - 1.96 * se,
            as.numeric(tau_hat) + 1.96 * se))

# ── Plots ─────────────────────────────────────────────────────────────────────
plot(tau_hat)
