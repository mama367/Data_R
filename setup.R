# =============================================================================
# R Programming Best Practices Setup
# =============================================================================

# 1. REPOSITORY CONFIGURATION
# Set CRAN mirror for package downloads (use cloud mirror for reliability)
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Alternative: Set multiple repositories
# options(repos = c(
#   CRAN = "https://cloud.r-project.org/",
#   BioConductor = "https://bioconductor.org/packages/release/bioc"
# ))

# 2. WORKING DIRECTORY SETUP
# Set working directory (adjust path as needed)
# Use forward slashes or double backslashes on Windows
# setwd("~/Dropbox/2025BigData")

# Alternative: Use here package for project-relative paths
# library(here)
# setwd(here())

# 3. SESSION CONFIGURATION
# Disable automatic conversion of strings to factors
options(stringsAsFactors = FALSE)

# Set default number of digits for numeric output
options(digits = 4)

# Increase memory limit if working with large datasets
# options(java.parameters = "-Xmx8g")  # For rJava-dependent packages

# 4. PACKAGE MANAGEMENT
# Define required packages
required_packages <- c(
  "tidyverse",    # Data manipulation and visualization (includes dplyr, ggplot2, etc.)
  "readr",        # Fast reading of delimited files
  "readxl",       # Read Excel files
  "haven",        # Read SPSS, Stata, and SAS files
  "stringr",      # String manipulation
  "forcats",      # Factor manipulation
  "lubridate",    # Date and time manipulation
  "dplyr",        # Data manipulation
  "ggplot2",      # Data visualization
  "gridExtra",    # Arrange multiple plots
  "grid",         # Low-level graphics
  "knitr",        # Dynamic report generation
  "rmarkdown",    # R Markdown documents
  "DT",           # Interactive tables
  "plotly",       # Interactive plots
  "tidytext",
  "showtext"      # korean font
)

# Function to install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}

# Install and load all required packages
install_and_load(required_packages)

# Alternative: Using pacman for package management
# if (!require(pacman)) install.packages("pacman")
# pacman::p_load(tidyverse, readr, stringr, ggplot2, forcats, 
#                gridExtra, grid, haven, readxl, lubridate)

# 5. Set ggplot2 theme for consistent visualization
theme_set(theme_minimal())

# Set default figure dimensions for plots
options(repr.plot.width = 10, repr.plot.height = 6)

# 6. UTILITY FUNCTIONS
# Function to clear environment except specified objects
clear_env <- function(keep = c()) {
  to_remove <- setdiff(ls(envir = .GlobalEnv), keep)
  rm(list = to_remove, envir = .GlobalEnv)
}

# Function to display session information
show_session_info <- function() {
  cat("R Version:", R.version.string, "\n")
  cat("Working Directory:", getwd(), "\n")
  cat("Loaded Packages:\n")
  print(sessionInfo())
}

# DATA IMPORT HELPER FUNCTIONS
# Function for robust CSV reading with Korean encoding
read_csv_robust <- function(file, ...) {
  tryCatch({
    readr::read_csv(file, locale = locale(encoding = "UTF-8"), ...)
  }, error = function(e) {
    message("UTF-8 encoding failed, trying with EUC-KR for Korean files...")
    tryCatch({
      readr::read_csv(file, locale = locale(encoding = "EUC-KR"), ...)
    }, error = function(e2) {
      message("EUC-KR encoding failed, trying with CP949...")
      readr::read_csv(file, locale = locale(encoding = "CP949"), ...)
    })
  })
}

# 7. GLOBAL SETTINGS
# Korean Font Configuration for proper Hangul display
# Check operating system and set appropriate Korean fonts
if (Sys.info()["sysname"] == "Darwin") {  # macOS
  korean_font <- "AppleGothic"  # or "Apple SD Gothic Neo"
} else if (Sys.info()["sysname"] == "Windows") {  # Windows
  korean_font <- "Malgun Gothic"  
} else {  # Linux
  korean_font <- "NanumGothic"  # Requires Nanum fonts installation
}

# Set ggplot2 theme with Korean font support
theme_set(theme_minimal(base_family = korean_font))

# Configure Korean locale and encoding
if (Sys.info()["sysname"] == "Windows") {
  Sys.setlocale("LC_ALL", "Korean")
} else {
  Sys.setlocale("LC_ALL", "ko_KR.UTF-8")
}

# Set default figure dimensions for plots
options(repr.plot.width = 10, repr.plot.height = 6)

# Function to create Korean-compatible ggplot theme
theme_korean <- function(base_size = 12, base_family = korean_font) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(family = base_family),
      plot.title = element_text(family = base_family, hjust = 0.5),
      axis.title = element_text(family = base_family),
      legend.text = element_text(family = base_family),
      legend.title = element_text(family = base_family),
      strip.text = element_text(family = base_family)
    )
}

# Function for reading Korean Excel files
read_excel_korean <- function(file, ...) {
  tryCatch({
    readxl::read_excel(file, ...)
  }, error = function(e) {
    message("Reading Excel file with Korean locale settings...")
    readxl::read_excel(file, locale = locale(encoding = "CP949"), ...)
  })
}

## R base 그래프 한글 사용 설정하기
# font 등록
font_add_google(name = "Noto Serif KR",
                family = "noto-serif")
showtext_auto(TRUE)

# DISPLAY STARTUP MESSAGE
cat("=====================================\n")
cat("R Environment Setup Complete\n")
cat("Working Directory:", getwd(), "\n")
cat("CRAN Mirror:", getOption("repos")["CRAN"], "\n")
cat("Korean Font:", korean_font, "\n")
cat("Locale:", Sys.getlocale("LC_ALL"), "\n")
cat("Packages Loaded:", length(required_packages), "\n")
cat("=====================================\n")
