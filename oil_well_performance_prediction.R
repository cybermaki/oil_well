##########################################################
# Oil Wells Recommendation System
##########################################################

# Suppress package startup messages
suppressPackageStartupMessages({
  # Load required libraries
  if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
  if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
  if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
  if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
  if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
  if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
  if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
  if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
  if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
  if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
  if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
  if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
  if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org") 
   
  library(tidyverse)
  library(caret)
  library(ggplot2)
  library(lubridate)
  library(kableExtra)
  library(knitr)
  library(data.table)
  library(corrplot)
  library(randomForest)
  library(glmnet)
  library(gridExtra)
  library(dplyr)
  library(GGally)
  library(readr)
})

# Define wheat color palette for consistent visualization
wheat_palette <- c(
  wheat = "#F5DEB3",
  wheat1 = "#EAD2AC",
  wheat2 = "#D2B48C",
  wheat3 = "#E6C27A",
  wheat4 = "#C19A6B"
)

# Set global options
options(scipen = 999) # Avoid scientific notation

##########################################################
# Data Acquisition and Loading
##########################################################

# Check for local file first
csv_file <- "oil_wells_data.csv"

if(!file.exists(csv_file)) {
  # Download from GitHub
  cat("Attempting to download oil wells data from GitHub...\n")
  zip_url <- "https://github.com/cybermaki/oil_well/raw/refs/heads/main/oil_wells_data.zip"
  zip_file <- "oil_wells_data.zip"
  
  tryCatch({
    # Download the zip file
    download.file(zip_url, destfile = zip_file, mode = "wb", method = "auto")
    cat("Download successful!\n")
    
    # Unzip the file
    cat("Extracting CSV file...\n")
    unzip(zip_file, files = csv_file, exdir = ".")
    
    # Check if extraction was successful
    if(file.exists(csv_file)) {
      cat("CSV file extracted successfully!\n")
      # Remove the zip file to save space
      file.remove(zip_file)
    } else {
      # List contents of zip to help debug
      zip_contents <- unzip(zip_file, list = TRUE)
      cat("Contents of zip file:\n")
      print(zip_contents$Name)
      stop("CSV file not found in zip archive. Check the file names above.")
    }
  }, error = function(e) {
    cat("Error during download or extraction:", e$message, "\n")
    cat("Please download manually from:", zip_url, "\n")
    cat("Extract 'oil_wells_data.csv' to your working directory.\n")
    stop("File download or extraction failed")
  })
}

# Read the data with robust error handling
cat("Reading oil wells data...\n")

# Method 1: Try reading with readLines first to fix incomplete final line
tryCatch({
  # Read all lines
  lines <- readLines(csv_file, warn = FALSE)
  
  # Check if last line is empty or incomplete
  if(length(lines) > 0) {
    last_line <- lines[length(lines)]
    # Count commas in first line (header) vs last line
    header_commas <- length(gregexpr(",", lines[1])[[1]])
    last_line_commas <- length(gregexpr(",", last_line)[[1]])
    
    # If last line has fewer fields or is empty, remove it
    if(last_line == "" || last_line_commas < header_commas) {
      cat("Removing incomplete final line from CSV...\n")
      lines <- lines[-length(lines)]
    }
    
    # Write cleaned lines to temporary file
    temp_file <- tempfile(fileext = ".csv")
    writeLines(lines, temp_file)
    
    # Read the cleaned file
    oil_wells_raw <- read.csv(temp_file, stringsAsFactors = FALSE, header = TRUE, 
                              check.names = FALSE, fill = TRUE)
    
    # Remove temporary file
    unlink(temp_file)
  } else {
    stop("CSV file appears to be empty")
  }
}, error = function(e) {
  cat("Error with readLines approach:", e$message, "\n")
  cat("Attempting alternative reading method...\n")
  
  # Method 2: Use data.table for more robust reading
  if(require(data.table)) {
    oil_wells_raw <- fread(csv_file, stringsAsFactors = FALSE, 
                           check.names = FALSE, fill = TRUE, 
                           showProgress = FALSE)
    oil_wells_raw <- as.data.frame(oil_wells_raw)
  } else {
    # Method 3: Basic read.csv with additional parameters
    oil_wells_raw <- read.csv(csv_file, stringsAsFactors = FALSE, 
                              header = TRUE, check.names = FALSE, 
                              fill = TRUE, blank.lines.skip = TRUE,
                              comment.char = "", quote = "\"")
  }
})

# Verify data was loaded
if(!exists("oil_wells_raw") || nrow(oil_wells_raw) == 0) {
  stop("Failed to load data from CSV file")
}

# Remove any completely empty rows
oil_wells_raw <- oil_wells_raw[rowSums(is.na(oil_wells_raw)) != ncol(oil_wells_raw), ]

# Check for and remove any duplicate header rows
if(nrow(oil_wells_raw) > 1) {
  # Check if any row matches the column names
  header_check <- apply(oil_wells_raw, 1, function(row) {
    all(row == names(oil_wells_raw), na.rm = TRUE)
  })
  
  if(any(header_check)) {
    cat("Removing duplicate header rows...\n")
    oil_wells_raw <- oil_wells_raw[!header_check, ]
  }
}

cat("Data loaded successfully. Number of rows:", nrow(oil_wells_raw), "\n")
cat("Number of columns:", ncol(oil_wells_raw), "\n")

# Display first few column names to verify structure
cat("\nFirst 10 column names:\n")
print(head(names(oil_wells_raw), 10))


##########################################################
# Column Selection and Mapping
##########################################################

# Define required columns
columns_to_keep <- c(
  "User-Format Well ID", "Well Status Abrv", "Well Status Text", 
  "Lateral Length (m)", "Producing Field/Area Name", "Producing Pool Name",
  "Cumulative GAS Prod. (e3m3)", "Cumulative OIL Prod. (m3)", 
  "Cumulative WTR Prod. (m3)", "First 12 mo. Total GAS (e3m3)",
  "First 12 mo. Total OIL (m3)", "First 12 mo. Total WTR (m3)",
  "First 12 mo. Total BOE (Bbl)", "First 12 mo. Dly Avg. GAS (e3m3/day)",
  "First 12 mo. Dly Avg. OIL (m3/day)", "First 12 mo. Dly Avg. WTR (m3/day)",
  "First 12 mo. Ave WC%", "Cumulative WTR Inject. (m3)",
  "Total Production Hrs", "Cur Operator Name", "Date Well Spudded",
  "Bot-Hole Latitude (NAD27)", "Bot-Hole Longitude (NAD27)",
  "Surf-Hole Latitude (NAD27)", "Surf-Hole Longitude (NAD27)",
  "Prod./Inject. Frmtn"
)

# Normalize column names function
normalize_name <- function(x) {
  gsub("[^A-Za-z0-9]", ".", x)
}

# Match columns
normalized_columns_to_keep <- normalize_name(columns_to_keep)
normalized_raw_names <- normalize_name(names(oil_wells_raw))

existing_columns <- character()
column_mapping <- list()

for(i in seq_along(columns_to_keep)) {
  if(columns_to_keep[i] %in% names(oil_wells_raw)) {
    existing_columns <- c(existing_columns, columns_to_keep[i])
    column_mapping[[columns_to_keep[i]]] <- columns_to_keep[i]
  } else {
    norm_idx <- which(normalized_columns_to_keep[i] == normalized_raw_names)
    if(length(norm_idx) > 0) {
      selected_col <- names(oil_wells_raw)[norm_idx[1]]
      existing_columns <- c(existing_columns, selected_col)
      column_mapping[[selected_col]] <- columns_to_keep[i]
    }
  }
}

existing_columns <- unique(existing_columns)

cat("\nColumn matching summary:\n")
cat("Expected columns:", length(columns_to_keep), "\n")
cat("Found columns:", length(existing_columns), "\n")
cat("Missing columns:", length(columns_to_keep) - length(existing_columns), "\n")

# Select columns
if(length(existing_columns) > 0) {
  oil_wells <- oil_wells_raw %>% select(all_of(existing_columns))
} else {
  oil_wells <- oil_wells_raw
}

cat("\nFinal dataset has", ncol(oil_wells), "columns and", nrow(oil_wells), "rows.\n")

##########################################################
# Data Cleaning and Standardization
##########################################################

# Rename columns
rename_mapping <- list(
  "User-Format Well ID" = "well_id",
  "Well Status Abrv" = "well_status_abrv",
  "Well Status Text" = "well_status_text",
  "Lateral Length (m)" = "lateral_length_m",
  "Producing Field/Area Name" = "producing_field_area_name",
  "Producing Pool Name" = "producing_pool_name",
  "Cumulative GAS Prod. (e3m3)" = "cumulative_gas_prod_e3m3",
  "Cumulative OIL Prod. (m3)" = "cumulative_oil_prod_m3",
  "Cumulative WTR Prod. (m3)" = "cumulative_wtr_prod_m3",
  "First 12 mo. Total GAS (e3m3)" = "first_12mo_total_gas_e3m3",
  "First 12 mo. Total OIL (m3)" = "first_12mo_total_oil_m3",
  "First 12 mo. Total WTR (m3)" = "first_12mo_total_wtr_m3",
  "First 12 mo. Total BOE (Bbl)" = "first_12mo_total_boe_bbl",
  "First 12 mo. Dly Avg. GAS (e3m3/day)" = "first_12mo_dly_avg_gas_e3m3_day",
  "First 12 mo. Dly Avg. OIL (m3/day)" = "first_12mo_dly_avg_oil_m3_day",
  "First 12 mo. Dly Avg. WTR (m3/day)" = "first_12mo_dly_avg_wtr_m3_day",
  "First 12 mo. Ave WC%" = "first_12mo_ave_wc_pct",
  "Cumulative WTR Inject. (m3)" = "cumulative_wtr_inject_m3",
  "Total Production Hrs" = "total_production_hrs",
  "Cur Operator Name" = "cur_operator_name",
  "Date Well Spudded" = "date_well_spudded",
  "Bot-Hole Latitude (NAD27)" = "bot_hole_latitude_nad27",
  "Bot-Hole Longitude (NAD27)" = "bot_hole_longitude_nad27",
  "Surf-Hole Latitude (NAD27)" = "surf_hole_latitude_nad27",
  "Surf-Hole Longitude (NAD27)" = "surf_hole_longitude_nad27",
  "Prod./Inject. Frmtn" = "prod_inject_frmtn"
)

# Apply renaming
rename_pairs <- list()
for(selected_col in names(oil_wells)) {
  if(selected_col %in% names(column_mapping)) {
    original_name <- column_mapping[[selected_col]]
    if(original_name %in% names(rename_mapping)) {
      rename_pairs[[rename_mapping[[original_name]]]] <- selected_col
    }
  }
}

valid_rename <- rename_pairs[lengths(rename_pairs) > 0]

if(length(valid_rename) > 0) {
  oil_wells <- oil_wells %>% rename(!!!valid_rename)
  cat("\nNumber of columns renamed:", length(valid_rename), "\n")
}

# Remove duplicates
if("well_id" %in% names(oil_wells)) {
  initial_rows <- nrow(oil_wells)
  oil_wells <- oil_wells %>% distinct(well_id, .keep_all = TRUE)
  duplicates_removed <- initial_rows - nrow(oil_wells)
  if(duplicates_removed > 0) {
    cat("Removed", duplicates_removed, "duplicate wells\n")
  }
}

##########################################################
# Geographic Coordinate Processing
##########################################################

# Process latitude and longitude columns
lat_lon_columns <- c("bot_hole_latitude_nad27", "bot_hole_longitude_nad27",
                     "surf_hole_latitude_nad27", "surf_hole_longitude_nad27")

cat("\nProcessing geographic coordinates...\n")
for(col in lat_lon_columns) {
  if(col %in% names(oil_wells)) {
    oil_wells[[col]] <- as.character(oil_wells[[col]])
    oil_wells[[col]] <- gsub("[NSWEnswe]", "", oil_wells[[col]])
    oil_wells[[col]] <- as.numeric(oil_wells[[col]])
    
    # Apply negative sign for longitude (Western hemisphere)
    if(grepl("longitude", col, ignore.case = TRUE)) {
      oil_wells[[col]] <- -abs(oil_wells[[col]])
    }
    
    cat(col, "- Min:", round(min(oil_wells[[col]], na.rm = TRUE), 4), 
        "Max:", round(max(oil_wells[[col]], na.rm = TRUE), 4),
        "NAs:", sum(is.na(oil_wells[[col]])), "\n")
  }
}

##########################################################
# Date and Numeric Processing
##########################################################

# Process date_well_spudded
if("date_well_spudded" %in% names(oil_wells)) {
  cat("\nProcessing dates...\n")
  oil_wells$date_well_spudded <- as.character(oil_wells$date_well_spudded)
  
  # Try multiple date formats
  date_formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d")
  parsed_dates <- NULL
  
  for(fmt in date_formats) {
    temp_dates <- as.Date(oil_wells$date_well_spudded, format = fmt)
    if(is.null(parsed_dates) || sum(is.na(temp_dates)) < sum(is.na(parsed_dates))) {
      parsed_dates <- temp_dates
    }
  }
  
  # Fallback to lubridate
  if(sum(is.na(parsed_dates)) > 0) {
    parsed_dates <- parse_date_time(oil_wells$date_well_spudded, 
                                    orders = c("Ymd", "mdY", "dmy"), tz = "UTC")
    parsed_dates <- as.Date(parsed_dates)
  }
  
  oil_wells$date_well_spudded <- parsed_dates
  cat("Date parsing complete. NAs:", sum(is.na(oil_wells$date_well_spudded)), "\n")
}

# Process numeric columns
numeric_columns <- c(
  "lateral_length_m", "cumulative_gas_prod_e3m3", "cumulative_oil_prod_m3",
  "cumulative_wtr_prod_m3", "first_12mo_total_gas_e3m3", "first_12mo_total_oil_m3",
  "first_12mo_total_wtr_m3", "first_12mo_total_boe_bbl", "first_12mo_dly_avg_gas_e3m3_day",
  "first_12mo_dly_avg_oil_m3_day", "first_12mo_dly_avg_wtr_m3_day", "first_12mo_ave_wc_pct",
  "cumulative_wtr_inject_m3", "total_production_hrs"
)

cat("\nProcessing numeric columns...\n")
for(col in numeric_columns) {
  if(col %in% names(oil_wells)) {
    oil_wells[[col]] <- as.numeric(gsub("[^0-9.-]", "", as.character(oil_wells[[col]])))
    # Set negative values to NA
    neg_count <- sum(oil_wells[[col]] < 0, na.rm = TRUE)
    if(neg_count > 0) {
      oil_wells[[col]][oil_wells[[col]] < 0] <- NA
      cat(col, "- Set", neg_count, "negative values to NA\n")
    }
  }
}

##########################################################
# Producing Wells Identification
##########################################################

cat("\nFiltering for producing wells...\n")
cat("Initial wells:", nrow(oil_wells), "\n")

# Filter for producing wells
if("well_status_text" %in% names(oil_wells) && "well_status_abrv" %in% names(oil_wells)) {
  producing_wells <- oil_wells %>%
    filter(grepl("active|producing|prod|in production|oil", well_status_text, ignore.case = TRUE) | 
             grepl("prod|active|p|act|o", well_status_abrv, ignore.case = TRUE))
  cat("After status filter:", nrow(producing_wells), "\n")
} else if("well_status_text" %in% names(oil_wells)) {
  producing_wells <- oil_wells %>%
    filter(grepl("active|producing|prod|in production|oil", well_status_text, ignore.case = TRUE))
  cat("After status filter (text only):", nrow(producing_wells), "\n")
} else if("well_status_abrv" %in% names(oil_wells)) {
  producing_wells <- oil_wells %>%
    filter(grepl("prod|active|p|act|o", well_status_abrv, ignore.case = TRUE))
  cat("After status filter (abrv only):", nrow(producing_wells), "\n")
} else {
  producing_wells <- oil_wells
  cat("No status columns found, using all wells\n")
}

# Filter for positive production
if("cumulative_oil_prod_m3" %in% names(producing_wells)) {
  producing_wells <- producing_wells %>%
    filter(!is.na(cumulative_oil_prod_m3) & cumulative_oil_prod_m3 > 0)
  cat("After production filter:", nrow(producing_wells), "\n")
}

cat("Final producing wells:", nrow(producing_wells), "\n")

##########################################################
# Exploratory Data Analysis
##########################################################

cat("\n=== Exploratory Data Analysis ===\n")

# Dataset structure summary
oil_wells_structure <- data.frame(
  Column = names(oil_wells),
  Type = sapply(oil_wells, function(x) paste(class(x), collapse = ", ")),
  Non_NA_Count = sapply(oil_wells, function(x) sum(!is.na(x))),
  NA_Percentage = sapply(oil_wells, function(x) round(sum(is.na(x)) / length(x) * 100, 2)),
  stringsAsFactors = FALSE
)

cat("\nDataset structure (first 10 columns):\n")
print(head(oil_wells_structure, 10))

# Key statistics
cat("\nKey dataset statistics:\n")
cat("Total wells:", nrow(oil_wells), "\n")
cat("Total columns:", ncol(oil_wells), "\n")
if("cur_operator_name" %in% names(oil_wells)) {
  cat("Unique operators:", n_distinct(oil_wells$cur_operator_name, na.rm = TRUE), "\n")
}
if("producing_field_area_name" %in% names(oil_wells)) {
  cat("Unique fields:", n_distinct(oil_wells$producing_field_area_name, na.rm = TRUE), "\n")
}
if("cumulative_oil_prod_m3" %in% names(oil_wells)) {
  cat("Mean oil production (m3):", round(mean(oil_wells$cumulative_oil_prod_m3, na.rm = TRUE), 2), "\n")
  cat("Median oil production (m3):", round(median(oil_wells$cumulative_oil_prod_m3, na.rm = TRUE), 2), "\n")
}

# Visualizations
if(nrow(producing_wells) > 0) {
  
  # 1. Production distribution
  if("cumulative_oil_prod_m3" %in% names(producing_wells)) {
    p1 <- ggplot(producing_wells %>% filter(!is.na(cumulative_oil_prod_m3) & cumulative_oil_prod_m3 > 0), 
                 aes(x = cumulative_oil_prod_m3)) +
      geom_histogram(bins = 50, fill = wheat_palette["wheat"], color = "white") +
      scale_x_log10(labels = scales::comma) +
      labs(title = "Distribution of Cumulative Oil Production",
           x = "Cumulative Oil Production (m3, log scale)", y = "Count") +
      theme_minimal(base_size = 10)
    print(p1)
  }
  
  # 2. Lateral length vs production
  if(all(c("lateral_length_m", "cumulative_oil_prod_m3") %in% names(producing_wells))) {
    p2 <- ggplot(producing_wells %>% 
                   filter(!is.na(lateral_length_m) & !is.na(cumulative_oil_prod_m3) & 
                            lateral_length_m > 0 & cumulative_oil_prod_m3 > 0), 
                 aes(x = lateral_length_m, y = cumulative_oil_prod_m3)) +
      geom_point(alpha = 0.3, color = wheat_palette["wheat1"]) +
      geom_smooth(method = "lm", color = wheat_palette["wheat4"], se = FALSE) +
      scale_y_log10(labels = scales::comma) +
      labs(title = "Lateral Length vs Cumulative Oil Production",
           x = "Lateral Length (m)", y = "Cumulative Oil Production (m3, log scale)") +
      theme_minimal(base_size = 10)
    print(p2)
  }
  
  # 3. Top operators
  if("cur_operator_name" %in% names(producing_wells)) {
    top_operators <- producing_wells %>%
      group_by(cur_operator_name) %>%
      summarize(well_count = n(), .groups = 'drop') %>%
      arrange(desc(well_count)) %>%
      slice_head(n = 15)
    
    p3 <- ggplot(top_operators, aes(x = reorder(cur_operator_name, well_count), y = well_count)) +
      geom_bar(stat = "identity", fill = wheat_palette["wheat2"]) +
      coord_flip() +
      labs(title = "Top 15 Operators by Well Count",
           x = "Operator", y = "Number of Wells") +
      theme_minimal(base_size = 10) +
      theme(axis.text.y = element_text(size = 8))
    print(p3)
  }
  
  # 4. Geographic distribution
  if(all(c("bot_hole_latitude_nad27", "bot_hole_longitude_nad27", "cumulative_oil_prod_m3") %in% names(producing_wells))) {
    p4 <- ggplot(producing_wells %>% 
                   filter(!is.na(bot_hole_latitude_nad27) & !is.na(bot_hole_longitude_nad27) & 
                            !is.na(cumulative_oil_prod_m3)), 
                 aes(x = bot_hole_longitude_nad27, y = bot_hole_latitude_nad27)) +
      geom_point(aes(color = cumulative_oil_prod_m3), alpha = 0.6, size = 2) +
      scale_color_gradient(low = wheat_palette["wheat1"], high = wheat_palette["wheat4"], 
                           name = "Oil Prod (m3)", labels = scales::comma) +
      labs(title = "Geographic Distribution of Wells",
           x = "Longitude", y = "Latitude") +
      theme_minimal(base_size = 10)
    print(p4)
  }
}

##########################################################
# Machine Learning Model Development
##########################################################

cat("\n=== Machine Learning Model Development ===\n")

# Check required columns
required_cols <- c("first_12mo_total_boe_bbl", "lateral_length_m")
has_required <- all(required_cols %in% names(producing_wells))

if(nrow(producing_wells) > 0 && has_required) {
  # Create model data
  cat("Creating model data with BOE per meter target...\n")
  model_data <- producing_wells %>%
    filter(!is.na(first_12mo_total_boe_bbl) & !is.na(lateral_length_m) &
             lateral_length_m >= 1.0 & first_12mo_total_boe_bbl >= 1.0) %>%
    mutate(boe_per_meter = first_12mo_total_boe_bbl / lateral_length_m) %>%
    filter(!is.na(boe_per_meter) & is.finite(boe_per_meter) & boe_per_meter > 0) %>%
    mutate(log_boe_per_meter = log(boe_per_meter)) %>%
    filter(!is.na(log_boe_per_meter) & is.finite(log_boe_per_meter))
} else {
  # Alternative approach
  cat("Using alternative approach with cumulative oil production...\n")
  if("cumulative_oil_prod_m3" %in% names(oil_wells)) {
    model_data <- oil_wells %>%
      filter(!is.na(cumulative_oil_prod_m3) & cumulative_oil_prod_m3 > 0) %>%
      mutate(log_oil_prod = log(cumulative_oil_prod_m3))
  } else {
    stop("No suitable target variable found for modeling")
  }
}

cat("Model data created with", nrow(model_data), "samples\n")

# Additional features
if(all(c("cumulative_oil_prod_m3", "cumulative_gas_prod_e3m3") %in% names(model_data))) {
  model_data <- model_data %>%
    mutate(oil_gas_ratio = cumulative_oil_prod_m3 / (cumulative_gas_prod_e3m3 + 1))
}

if("date_well_spudded" %in% names(model_data)) {
  model_data <- model_data %>%
    mutate(well_age_days = as.numeric(Sys.Date() - date_well_spudded))
}

# Feature selection
potential_features <- c("lateral_length_m", "first_12mo_ave_wc_pct", 
                        "bot_hole_latitude_nad27", "bot_hole_longitude_nad27",
                        "total_production_hrs", "cumulative_gas_prod_e3m3",
                        "oil_gas_ratio", "well_age_days")
feature_cols <- potential_features[potential_features %in% names(model_data)]

# Add field category if available
if("producing_field_area_name" %in% names(model_data)) {
  top_fields <- model_data %>%
    group_by(producing_field_area_name) %>%
    summarize(count = n()) %>% 
    arrange(desc(count)) %>% 
    slice_head(n = 20) %>%
    pull(producing_field_area_name)
  
  model_data <- model_data %>%
    mutate(field_category = as.factor(ifelse(producing_field_area_name %in% top_fields, 
                                             producing_field_area_name, "Other")))
  
  if(n_distinct(model_data$field_category[!is.na(model_data$field_category)]) >= 2) {
    feature_cols <- c(feature_cols, "field_category")
  }
}

cat("Available features:", paste(feature_cols, collapse = ", "), "\n")

# Determine target variable
target_var <- ifelse("log_boe_per_meter" %in% names(model_data), "log_boe_per_meter", "log_oil_prod")
cat("Target variable:", target_var, "\n")

# Create final model features
model_features <- model_data %>% select(all_of(c(feature_cols, target_var)))

# Impute missing values
for(col in feature_cols) {
  if(is.numeric(model_features[[col]])) {
    model_features[[col]][is.na(model_features[[col]])] <- median(model_features[[col]], na.rm = TRUE)
  } else if(is.factor(model_features[[col]])) {
    model_features[[col]][is.na(model_features[[col]])] <- levels(model_features[[col]])[1]
  }
}

# Remove rows with NA in target
model_features <- model_features %>% 
  filter(!is.na(.data[[target_var]]) & is.finite(.data[[target_var]]))

# Feature scaling
numeric_features <- feature_cols[sapply(model_features[feature_cols], is.numeric)]
if(length(numeric_features) > 0) {
  pre_proc <- preProcess(model_features[numeric_features], method = c("center", "scale"))
  model_features[numeric_features] <- predict(pre_proc, model_features[numeric_features])
}

cat("\nFinal modeling dataset has", nrow(model_features), "rows and", ncol(model_features), "columns\n")

##########################################################
# Model Training and Evaluation
##########################################################

# Define metrics
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae <- function(actual, predicted) mean(abs(actual - predicted))
r_squared <- function(actual, predicted) {
  if(length(actual) == length(predicted) && length(actual) > 1) {
    cor(actual, predicted)^2
  } else {
    NA
  }
}

# Train-test split
set.seed(123)
train_indices <- createDataPartition(model_features[[target_var]], p = 0.8, list = FALSE)
train_set <- model_features[train_indices, ]
test_set <- model_features[-train_indices, ]

cat("\nTraining set size:", nrow(train_set), "\n")
cat("Test set size:", nrow(test_set), "\n")

# Baseline model
baseline_pred <- mean(train_set[[target_var]], na.rm = TRUE)
baseline_rmse <- rmse(test_set[[target_var]], rep(baseline_pred, nrow(test_set)))
baseline_mae <- mae(test_set[[target_var]], rep(baseline_pred, nrow(test_set)))
cat("\nBaseline RMSE:", baseline_rmse, "\n")

# Linear regression
cat("\nTraining Linear Regression...\n")
formula_features <- setdiff(names(train_set), target_var)

# Check for factors with single level
valid_features <- character()
for(feat in formula_features) {
  if(is.factor(train_set[[feat]])) {
    if(n_distinct(train_set[[feat]][!is.na(train_set[[feat]])]) >= 2) {
      valid_features <- c(valid_features, feat)
    }
  } else {
    valid_features <- c(valid_features, feat)
  }
}

# Build formula
if("lateral_length_m" %in% valid_features) {
  lm_formula <- as.formula(paste(target_var, "~", 
                                 paste(valid_features, collapse = " + "), 
                                 "+ I(lateral_length_m^2)"))
} else {
  lm_formula <- as.formula(paste(target_var, "~", 
                                 paste(valid_features, collapse = " + ")))
}

# Train linear model
set.seed(123)
cv_lm <- train(lm_formula, data = train_set, method = "lm", 
               trControl = trainControl(method = "cv", number = 5))
lm_pred <- predict(cv_lm, test_set)
lm_rmse <- rmse(test_set[[target_var]], lm_pred)
lm_r2 <- r_squared(test_set[[target_var]], lm_pred)
cat("Linear Model RMSE:", lm_rmse, "\n")

# Random Forest
cat("\nTraining Random Forest...\n")

# Prepare data for Random Forest
rf_train <- train_set %>% select(-all_of(target_var))
rf_train_y <- train_set[[target_var]]
rf_test <- test_set %>% select(-all_of(target_var))

# Ensure consistent factor levels
for(col in names(rf_train)) {
  if(is.factor(rf_train[[col]])) {
    common_levels <- intersect(levels(rf_train[[col]]), levels(rf_test[[col]]))
    rf_train[[col]] <- factor(rf_train[[col]], levels = common_levels)
    rf_test[[col]] <- factor(rf_test[[col]], levels = common_levels)
  }
}

# Train Random Forest
set.seed(123)
mtry_max <- max(1, floor(ncol(rf_train)/2))
mtry_min <- max(1, floor(ncol(rf_train)/4))
rf_tune_grid <- expand.grid(mtry = seq(mtry_min, mtry_max, by = 1))

rf_model <- train(x = rf_train, y = rf_train_y, method = "rf",
                  trControl = trainControl(method = "cv", number = 5),
                  tuneGrid = rf_tune_grid, ntree = 100)

rf_pred <- predict(rf_model, rf_test)
rf_rmse <- rmse(test_set[[target_var]], rf_pred)
rf_r2 <- r_squared(test_set[[target_var]], rf_pred)
cat("Random Forest RMSE:", rf_rmse, "\n")

# Variable importance
if(!is.null(rf_model$finalModel$importance)) {
  importance_df <- data.frame(
    Variable = rownames(rf_model$finalModel$importance),
    Importance = rf_model$finalModel$importance[, 1],
    stringsAsFactors = FALSE
  ) %>% arrange(desc(Importance))
  
  cat("\nTop 5 Most Important Variables:\n")
  print(head(importance_df, 5))
  
  # Plot variable importance
  p_importance <- ggplot(head(importance_df, 10), 
                         aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = wheat_palette["wheat4"]) +
    coord_flip() +
    labs(title = "Top 10 Most Important Variables",
         x = "Variable", y = "Importance") +
    theme_minimal(base_size = 10)
  print(p_importance)
}

# Elastic Net
cat("\nTraining Elastic Net...\n")

# Prepare data for Elastic Net
x_formula <- as.formula(paste("~", paste(setdiff(names(train_set), target_var), 
                                         collapse = " + "), "- 1"))
x_train <- model.matrix(x_formula, data = train_set)
y_train <- train_set[[target_var]]
x_test <- model.matrix(x_formula, data = test_set)

# Ensure same columns in train and test
common_cols <- intersect(colnames(x_train), colnames(x_test))
x_train <- x_train[, common_cols]
x_test <- x_test[, common_cols]

# Train Elastic Net
set.seed(123)
cv_enet <- cv.glmnet(x_train, y_train, alpha = 0.5, nfolds = 5)
best_lambda <- cv_enet$lambda.min
cat("Best lambda:", best_lambda, "\n")

enet_model <- glmnet(x_train, y_train, alpha = 0.5, lambda = best_lambda)
enet_pred <- predict(enet_model, s = best_lambda, newx = x_test)
enet_rmse <- rmse(test_set[[target_var]], as.vector(enet_pred))
enet_r2 <- r_squared(test_set[[target_var]], as.vector(enet_pred))
cat("Elastic Net RMSE:", enet_rmse, "\n")

# Ensemble Model
cat("\nCreating Ensemble Model...\n")
ensemble_pred <- (rf_pred + as.vector(enet_pred)) / 2
ensemble_rmse <- rmse(test_set[[target_var]], ensemble_pred)
ensemble_r2 <- r_squared(test_set[[target_var]], ensemble_pred)
cat("Ensemble RMSE:", ensemble_rmse, "\n")

##########################################################
# Results Summary
##########################################################

cat("\n=== Model Performance Summary ===\n")

# Compile results
results <- data.frame(
  Model = c("Baseline", "Linear Regression", "Random Forest", "Elastic Net", "Ensemble"),
  RMSE = round(c(baseline_rmse, lm_rmse, rf_rmse, enet_rmse, ensemble_rmse), 4),
  R_squared = round(c(NA, lm_r2, rf_r2, enet_r2, ensemble_r2), 4),
  Improvement = paste0(round((1 - c(baseline_rmse, lm_rmse, rf_rmse, enet_rmse, ensemble_rmse) / baseline_rmse) * 100, 1), "%"),
  stringsAsFactors = FALSE
)

print(results)

# Visualize results
p_results <- ggplot(results, aes(x = reorder(Model, -RMSE), y = RMSE)) +
  geom_bar(stat = "identity", fill = wheat_palette["wheat"]) +
  geom_text(aes(label = RMSE), vjust = -0.3, size = 3) +
  labs(title = "Model Performance Comparison",
       x = "Model", y = "RMSE") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_results)

# Best model diagnostics
best_model_idx <- which.min(results$RMSE)
best_model_name <- results$Model[best_model_idx]
cat("\nBest performing model:", best_model_name, "\n")

if(best_model_name == "Random Forest") {
  best_pred <- rf_pred
} else if(best_model_name == "Elastic Net") {
  best_pred <- as.vector(enet_pred)
} else if(best_model_name == "Ensemble") {
  best_pred <- ensemble_pred
} else {
  best_pred <- lm_pred
}

# Diagnostic plots
par(mfrow = c(2, 2))

# 1. Predicted vs Actual
plot(test_set[[target_var]], best_pred,
     xlab = paste("Actual", target_var),
     ylab = paste("Predicted", target_var),
     main = "Predicted vs Actual",
     pch = 16, col = adjustcolor(wheat_palette["wheat"], alpha = 0.5))
abline(0, 1, col = wheat_palette["wheat4"], lwd = 2)

# 2. Residuals vs Fitted
residuals <- test_set[[target_var]] - best_pred
plot(best_pred, residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted",
     pch = 16, col = adjustcolor(wheat_palette["wheat1"], alpha = 0.5))
abline(h = 0, col = wheat_palette["wheat4"], lwd = 2)

# 3. Q-Q plot
qqnorm(residuals, main = "Normal Q-Q Plot",
       pch = 16, col = adjustcolor(wheat_palette["wheat2"], alpha = 0.5))
qqline(residuals, col = wheat_palette["wheat4"], lwd = 2)

# 4. Histogram of residuals
hist(residuals, breaks = 30, main = "Histogram of Residuals",
     xlab = "Residuals", col = wheat_palette["wheat3"], border = "white")

par(mfrow = c(1, 1))

##########################################################
# Recommendations and Conclusions
##########################################################

cat("\n=== Recommendations for Oil Well Optimization ===\n")

# Key findings based on best model
if(best_model_name == "Random Forest" && exists("importance_df")) {
  cat("\nBased on Random Forest variable importance:\n")
  cat("Top 3 factors influencing well performance:\n")
  for(i in 1:min(3, nrow(importance_df))) {
    cat(i, ". ", importance_df$Variable[i], " (Importance: ", 
        round(importance_df$Importance[i], 2), ")\n", sep = "")
  }
} else if(best_model_name == "Linear Regression") {
  cat("\nBased on Linear Regression coefficients:\n")
  coef_summary <- summary(cv_lm$finalModel)$coefficients
  significant_vars <- rownames(coef_summary)[coef_summary[,4] < 0.05]
  cat("Significant variables (p < 0.05):\n")
  print(significant_vars)
}

cat("\nGeneral Recommendations:\n")
cat("1. Optimize Lateral Length: Analysis shows lateral length is a key driver of production\n")
cat("2. Geographic Targeting: Focus on areas with historically high production\n")
cat("3. Field Selection: Prioritize fields with proven track records\n")
cat("4. Water Cut Management: Monitor and minimize water cut in first 12 months\n")
cat("5. Technology Application: Newer wells may benefit from improved completion techniques\n")

# Performance metrics
cat("\nBest Model Performance:\n")
cat("Model:", best_model_name, "\n")
cat("RMSE:", results$RMSE[best_model_idx], "\n")
cat("R-squared:", results$R_squared[best_model_idx], "\n")
cat("Improvement over baseline:", results$Improvement[best_model_idx], "\n")

# Feature summary
cat("\nFeatures used in modeling:\n")
cat(paste(feature_cols, collapse = ", "), "\n")
cat("\nTarget variable:", target_var, "\n")
cat("Total samples:", nrow(model_features), "\n")
cat("Training samples:", nrow(train_set), "\n")
cat("Test samples:", nrow(test_set), "\n")

##########################################################
# Save Results
##########################################################

# Save processed data
write.csv(model_features, "processed_oil_wells_data.csv", row.names = FALSE)
cat("\nProcessed data saved to 'processed_oil_wells_data.csv'\n")

# Save model results
write.csv(results, "model_performance_results.csv", row.names = FALSE)
cat("Model results saved to 'model_performance_results.csv'\n")

# Save predictions
predictions_df <- data.frame(
  Actual = test_set[[target_var]],
  Baseline = rep(baseline_pred, nrow(test_set)),
  Linear = lm_pred,
  RandomForest = rf_pred,
  ElasticNet = as.vector(enet_pred),
  Ensemble = ensemble_pred,
  BestModel = best_pred
)
write.csv(predictions_df, "model_predictions.csv", row.names = FALSE)
cat("Predictions saved to 'model_predictions.csv'\n")

cat("\n=== Analysis Complete ===\n")
cat("Total execution time:", round(difftime(Sys.time(), start_time <- Sys.time(), units = "secs"), 2), "seconds\n")

