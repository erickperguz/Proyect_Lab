# -----------------------------------------------------------------------------
# Script: MX_Real_Rates_Analysis.R
# Project: The Real Rate Shield (Sisu Lab)
# Objective: Calculate historical Real Interest Rates in Mexico (CETES vs CPI)
# Author: Sisu Lab
# -----------------------------------------------------------------------------

# --- SETUP & LIBRARIES ---
# Note: Uncomment the install lines only if you don't have the packages.
# install.packages("tidyverse")
# install.packages("lubridate")

library(tidyverse)
library(lubridate)
library(readr)

# --- STEP 1: LOAD RAW DATA ---
# Important: Banxico data usually includes ~17 header lines. We skip them.
# Ensure your CSV files are in a folder named 'data' inside your project directory.

# Loading CETES (Treasury Certificates) and CPI (Consumer Price Index / INPC)
# Note: Using 'latin1' encoding to handle special characters in Spanish source files.
raw_cetes <- read_csv("./data/cetes_historico.csv", 
                      skip = 17, col_names = FALSE, locale = locale(encoding = "latin1"))

raw_cpi   <- read_csv("./data/inpc_historico.csv", 
                      skip = 17, col_names = FALSE, locale = locale(encoding = "latin1"))

# --- STEP 2: DATA CLEANING & PROCESSING ---

# 2.1 Clean CETES (Nominal Rates)
cetes_clean <- raw_cetes %>%
  select(X1, X2) %>%
  rename(date = X1, rate = X2) %>%
  
  # CRITICAL STEP: Convert text to numeric
  # This forces "N/E" (Not Existent) values into NAs. Warnings are expected.
  mutate(rate = as.numeric(rate)) %>% 
  mutate(date = dmy(date)) %>%
  
  # Filter out empty rows or parsing errors
  filter(!is.na(date), !is.na(rate)) %>% 
  
  # Standardize dates to the first day of the month for merging
  mutate(month_date = floor_date(date, "month")) %>%
  
  # Aggregate weekly auctions into a monthly average
  group_by(month_date) %>%
  summarise(nominal_rate = mean(rate, na.rm = TRUE))

print("--- PREVIEW: Monthly Nominal Rates (CETES) ---")
print(head(cetes_clean))

# 2.2 Clean CPI (Inflation Data)
cpi_clean <- raw_cpi %>%
  select(X1, X2) %>%
  rename(date = X1, cpi_value = X2) %>%
  
  # Type conversion and date parsing
  mutate(cpi_value = as.numeric(cpi_value)) %>% 
  mutate(date = dmy(date)) %>%
  filter(!is.na(date), !is.na(cpi_value)) %>%
  
  # Standardize to month
  mutate(month_date = floor_date(date, "month")) %>%
  arrange(month_date) %>%
  
  # CALCULATION: Year-over-Year (YoY) Inflation
  # Formula: (Current CPI / CPI 12 months ago) - 1
  mutate(inflation_yoy = (cpi_value / lag(cpi_value, 12) - 1) * 100) %>%
  
  # Remove the first 12 months (since lag calculation produces NAs)
  filter(!is.na(inflation_yoy)) %>%
  select(month_date, inflation_yoy)

print("--- PREVIEW: Clean CPI & Inflation Data ---")
print(head(cpi_clean))


# --- STEP 3: MERGE DATASETS ---
# Inner join keeps only the periods where we have both Rate and Inflation data
final_data <- inner_join(cetes_clean, cpi_clean, by = "month_date")


# --- STEP 4: REAL RATE CALCULATION (FISHER EQUATION) ---
# Exact Formula: r = ((1 + i) / (1 + π)) - 1
# Where: i = Nominal Rate, π = Inflation
final_data <- final_data %>%
  mutate(
    real_rate = ((1 + nominal_rate/100) / (1 + inflation_yoy/100) - 1) * 100,
    
    # Status Label for Visualization
    return_status = ifelse(real_rate > 0, "Real Gain", "Purchasing Power Loss")
  )

print("--- FINAL DATASET ---")
print(head(final_data))


# --- STEP 5: VISUALIZATION ---
ggplot(final_data, aes(x = month_date, y = real_rate)) +
  
  # Bars colored by status
  geom_col(aes(fill = return_status), width = 25) + 
  
  # Baseline at 0%
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  
  # Custom Colors: Green for Gain, Red for Loss
  scale_fill_manual(values = c(
    "Real Gain" = "#2ecc71", 
    "Purchasing Power Loss" = "#e74c3c"
  )) +
  
  # Professional Labeling
  labs(
    title = "Historical Real Rates in Mexico: CETES vs. Inflation",
    subtitle = "Ex-post Real Return Analysis (Fisher Equation)",
    x = "Year",
    y = "Real Rate (%)",
    fill = "Outcome",
    caption = "Source: Banxico | Analysis: Sisu Lab"
  ) +
  
  # Clean Theme
  theme_minimal() +
  theme(legend.position = "top")