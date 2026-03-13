library(readxl)
library(dplyr)
library(tidyr)
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

treat_date    <- as.Date("2022-09-01")
treat_time_id <- as.integer(format(treat_date, "%Y%m"))   # 202209

# ── Balance the panel ────────────────────────────────────────────────────────
# synthdid_estimate() requires a balanced panel (every unit × every period).
# Keep only stations observed in ALL time periods with no missing outcome.

total_months <- n_distinct(road_traffic$month)

balanced_nrs <- road_traffic %>%
  filter(!is.na(traffic_mo_su)) %>%
  group_by(nr) %>%
  summarise(n_months = n_distinct(month), .groups = "drop") %>%
  filter(n_months == total_months) %>%
  pull(nr)

# Split into treated (ZH/FR) and control stations
treated_balanced <- road_traffic %>%
  filter(nr %in% balanced_nrs, ct %in% c("ZH", "FR")) %>%
  distinct(nr) %>%
  pull(nr)

control_balanced <- setdiff(balanced_nrs, treated_balanced)

cat(sprintf(
  "Balanced panel: %d stations (%d treated ZH/FR, %d control), %d months\n",
  length(balanced_nrs), length(treated_balanced),
  length(control_balanced), total_months
))

# ── Build Y matrix manually ───────────────────────────────────────────────────
# Bypass panel.matrices() and construct Y, N0, T0 directly so we have full
# control over unit ordering (control rows first) and column ordering (time asc).
#
# Y  : (N0 + N1) × T matrix of outcomes
# N0 : number of control units  (rows 1 … N0)
# T0 : number of pre-treatment periods (columns 1 … T0)

Y_wide <- road_traffic %>%
  filter(nr %in% balanced_nrs, !is.na(traffic_mo_su)) %>%
  mutate(
    time       = as.integer(format(month, "%Y%m")),
    is_treated = nr %in% treated_balanced
  ) %>%
  select(nr, is_treated, time, traffic_mo_su) %>%
  arrange(is_treated, nr) %>%                    # control units first
  pivot_wider(names_from = time, values_from = traffic_mo_su,
              names_sort = TRUE)                  # columns in time order

Y  <- as.matrix(Y_wide %>% select(-nr, -is_treated))
rownames(Y) <- Y_wide$nr

N0 <- length(control_balanced)                   # number of control rows
T0 <- sum(as.integer(colnames(Y)) < treat_time_id)  # pre-treatment columns

cat(sprintf(
  "Matrix Y: %d rows (%d control + %d treated) × %d cols (%d pre + %d post)\n",
  nrow(Y), N0, nrow(Y) - N0, ncol(Y), T0, ncol(Y) - T0
))

# ── Estimation ───────────────────────────────────────────────────────────────
tau_hat <- synthdid_estimate(Y, N0, T0)

# Standard error via placebo variance method
se <- sqrt(vcov(tau_hat, method = "placebo"))

cat(sprintf("Synthetic DiD estimate : %+.1f vehicles/day\n", as.numeric(tau_hat)))
cat(sprintf("Std. error             :  %.1f\n",               se))
cat(sprintf("95%% CI                : (%+.1f, %+.1f)\n",
            as.numeric(tau_hat) - 1.96 * se,
            as.numeric(tau_hat) + 1.96 * se))

# ── Plots ─────────────────────────────────────────────────────────────────────
plot(tau_hat)
