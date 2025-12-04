###############################################
# TASK 04 – TRAFFIC ACCIDENT ANALYSIS (ROBUST)
###############################################

# Install & load libraries
packages <- c("tidyverse", "lubridate", "skimr", "hms")
install.packages(setdiff(packages, rownames(installed.packages())), quiet = TRUE)
library(tidyverse)
library(lubridate)
library(skimr)
library(hms)

# 1. Load dataset (ensure the file path is correct)
accident <- read.csv("accident.csv", stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A"))

# Quick preview
cat("---- HEAD ----\n"); print(head(accident))
cat("---- NAMES ----\n"); print(names(accident))

# Check required columns
required_cols <- c("Date","Time","Weather","Road_Condition","Severity")
missing_cols <- setdiff(required_cols, names(accident))
if(length(missing_cols) > 0){
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# Inspect unique / sample Time values to understand formats (useful for debugging)
cat("---- Sample Time values ----\n")
print(unique(head(accident$Time, 50)))

# Helper: safe parse Date that tries multiple formats
safe_parse_date <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "<NA>")] <- NA_character_
  # try lubridate fast functions in order
  out <- suppressWarnings(ymd(x))
  na_idx <- is.na(out) & !is.na(x)
  if(any(na_idx)) out[na_idx] <- suppressWarnings(dmy(x[na_idx]))
  na_idx <- is.na(out) & !is.na(x)
  if(any(na_idx)) out[na_idx] <- suppressWarnings(mdy(x[na_idx]))
  out
}

# Helper: robust time parser that returns hms or NA without error
safe_parse_time <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "<NA>")] <- NA_character_
  
  result <- rep(NA, length(x))
  
  # positions with full HH:MM:SS (e.g. 09:30:00)
  idx_hms <- grepl("^\\d{1,2}:\\d{2}:\\d{2}$", x)
  if(any(idx_hms)) {
    # wrap in tryCatch to avoid abort_lossy_cast
    result[idx_hms] <- tryCatch(
      as.character(as_hms(x[idx_hms])),
      error = function(e) {
        warning("Some HH:MM:SS values failed to parse; they will be set to NA")
        rep(NA_character_, sum(idx_hms))
      }
    )
  }
  
  # positions with HH:MM (e.g. 9:30 or 09:30)
  idx_hm <- grepl("^\\d{1,2}:\\d{2}$", x) & !idx_hms
  if(any(idx_hm)) {
    parsed <- suppressWarnings(hm(x[idx_hm]))   # lubridate::hm returns Period
    # convert Period to seconds then to hms
    result[idx_hm] <- as.character(as_hms(as.numeric(parsed)))
  }
  
  # optional: positions like "930" or "930AM" are not handled here; set to NA
  # return as hms vector (NA where parsing failed)
  # convert back to hms type
  out_hms <- rep(as_hms(NA), length(result))
  ok <- !is.na(result)
  if(any(ok)) out_hms[ok] <- as_hms(result[ok])
  out_hms
}

# 2. Data cleaning

# Date
accident$Date_parsed <- safe_parse_date(accident$Date)
bad_dates <- which(is.na(accident$Date_parsed) & !is.na(accident$Date))
if(length(bad_dates) > 0) {
  warning(paste0("There are ", length(bad_dates), " Date values that failed to parse. Check rows: ",
                 paste(head(bad_dates, 10), collapse = ", "), if(length(bad_dates)>10) " ..."))
}

# Time
accident$Time_parsed <- safe_parse_time(accident$Time)
bad_times <- which(is.na(accident$Time_parsed) & !is.na(accident$Time) & trimws(accident$Time) != "")
if(length(bad_times) > 0) {
  warning(paste0("There are ", length(bad_times), " Time values that failed to parse or are in unexpected format. Check rows: ",
                 paste(head(bad_times, 10), collapse = ", "), if(length(bad_times)>10) " ..."))
}

# Extract hour (numeric) safely
accident$Hour <- as.integer(hour(accident$Time_parsed))   # will be NA where Time_parsed is NA

# Replace missing weather / road condition
accident$Weather <- ifelse(is.na(accident$Weather) | trimws(accident$Weather) == "", "Unknown", accident$Weather)
accident$Road_Condition <- ifelse(is.na(accident$Road_Condition) | trimws(accident$Road_Condition) == "", "Unknown", accident$Road_Condition)

# Severity: convert to character then replace NAs
accident$Severity <- as.character(accident$Severity)
accident$Severity[is.na(accident$Severity) | trimws(accident$Severity) == ""] <- "Unknown"

cat("---- Missing values after cleaning (per column) ----\n")
print(colSums(is.na(accident)))

# 3. Quick EDA plots (won't error if columns exist)
# load ggplot2 from tidyverse
library(ggplot2)

# Accident Count by Weather
ggplot(accident, aes(x = Weather)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Accident Count by Weather", x = "Weather", y = "Count")

# Accident Count by Road Condition
ggplot(accident, aes(x = Road_Condition)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Accidents by Road Condition")

# Accidents by Hour of Day - filter out NA hours
ggplot(accident %>% filter(!is.na(Hour)), aes(x = Hour)) +
  geom_histogram(bins = 24) +
  labs(title = "Accidents by Hour of Day", x = "Hour", y = "Frequency")

# Severity distribution
ggplot(accident, aes(x = Severity, fill = Severity)) +
  geom_bar() +
  labs(title = "Accident Severity Distribution")

cat("---- TASK 04 (ROBUST) COMPLETED ----\n")