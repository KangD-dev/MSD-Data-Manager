#' @title Read CSV with Keyword as Header
#' 
#' @description
#' The `read_csv_with_keyword` function is designed to read a CSV file and start processing 
#' from the first row that contains a specified keyword. This row is treated as the header 
#' (column names), and all subsequent rows are considered data. The function is particularly 
#' useful when the data starts from an arbitrary row in the CSV file rather than the first row.
#' 
#' @param file_path Character. The path to the CSV file that needs to be read.
#' @param keyword Character. The keyword to search for in the CSV file. The function will identify 
#'   the first row containing this keyword and use it as the header.
#' @param search_limit Integer, optional. The number of initial rows to search for the keyword. 
#'   This parameter is useful for limiting the search to a specific portion of the file (e.g., 
#'   the first 10 rows). The default value is `10`.
#' 
#' @return A `data.frame` containing the data from the CSV file, with the header row set to the row 
#'   that contains the keyword. The data starts from the row immediately after the header.
#' 
#' @examples
#' # Example usage of the read_csv_with_keyword function
#' 
#' # Let's assume you have a CSV file named "data.csv" and the row containing the keyword "SampleID"
#' # is where you want the data to start. You want to search for the keyword within the first 10 rows.
#' 
#' # Read the CSV file with the keyword "SampleID"
#' data <- read_csv_with_keyword(file_path = "data.csv", keyword = "SampleID", search_limit = 10)
#' 
#' # View the first few rows of the processed data
#' head(data)
#' 
#' # Output: A data frame starting from the row where "SampleID" was found as the header,
#' # with all subsequent rows as the data.
#'
read_csv_with_keyword <- function(file_path, keyword, search_limit = 10) {
  # Read the CSV file, but only read the first 'search_limit' rows initially
  initial_data <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE, nrows = search_limit)
  
  # Find the row number where the keyword is located within the first 'search_limit' rows
  keyword_row <- which(apply(initial_data, 1, function(row) any(grepl(keyword, row, ignore.case = TRUE))))
  
  # If keyword is found within the search limit
  if (length(keyword_row) > 0) {
    keyword_row <- keyword_row[1]  # Take the first occurrence of the keyword
    
    # Read the entire file again, starting from the keyword row
    data <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE, skip = keyword_row - 1)
    
    # Extract the header (column names) from the row with the keyword
    header <- data[1, ]
    
    # Extract the data starting from the row after the header
    data <- data[-1, ]
    
    # Assign the correct column names
    colnames(data) <- header
    
    # Fix column names
    data <- clean_names(data)
    
    # Return the cleaned data as a data frame
    return(data)
  } else {
    stop(paste("Keyword not found in the first", search_limit, "rows of the file."))
  }
}


#' @title Format MSD EDR Data with QC
#'
#' @description
#' The `format_msd_raw_data` function processes raw MSD (Meso Scale Discovery) EDR data.
#' It includes QC steps to check for missing values, required columns, and duplicates before
#' formatting the data by extracting mean and unique results and combining them into a single data frame.
#'
#' @param raw_data A data frame containing the raw MSD EDR data.
#'
#' @return A data frame with formatted MSD EDR data, combining mean and unique results.
#' 
#' @examples
#' # Assuming `raw_data` is your input data frame containing MSD EDR data:
#' formatted_data <- format_msd_raw_data(raw_data)
#'
format_msd_raw_data <- function(raw_data) {
  
  # Define the fields required for the mean result report
  report_mean_fields <- c("sample", "assay", "spot", "dilution", "calc_conc_mean", "calc_conc_cv")
  
  # Define the fields required for the unique result report
  report_unique_fields <- c("sample", "assay", "well", "calc_concentration")
  
  # QC Step 1: Check for required columns
  required_columns <- c(report_mean_fields, report_unique_fields)
  missing_columns <- setdiff(required_columns, colnames(raw_data))
  
  if (length(missing_columns) > 0) {
    stop(paste("The following required columns are missing from the input data:", paste(missing_columns, collapse = ", ")))
  }
  
  # QC Step 2: Check for missing values in critical columns
  critical_columns <- c("sample", "assay", "calc_conc_mean", "calc_concentration")
  missing_values <- sapply(raw_data[critical_columns], function(col) sum(is.na(col)))
  
  if (any(missing_values > 0)) {
    warning("There are missing values in the following columns:")
    print(missing_values[missing_values > 0])
  }
  
  # QC Step 3: Check for duplicates in critical columns
  duplicate_rows <- raw_data %>% 
    select(sample, assay, well) %>% 
    duplicated()
  
  if (any(duplicate_rows)) {
    warning("There are duplicate rows in the input data based on sample, assay, and well columns.")
  }
  
  # Extract mean result data by selecting the relevant fields and removing duplicates
  mean_data <- raw_data %>%
    select(all_of(report_mean_fields)) %>%
    distinct() %>%
    mutate(calc_conc_mean = round(as.numeric(calc_conc_mean), 3),
           calc_conc_cv = round(as.numeric(calc_conc_cv), 3))
  
  # Extract unique result data
  unique_data <- raw_data %>%
    select(all_of(report_unique_fields)) %>%
    mutate(calc_concentration = round(as.numeric(calc_concentration), 3)) %>%
    group_by(sample, assay) %>%
    mutate(well = paste(well, collapse = "; "),
           calc_concentration = paste(calc_concentration, collapse = "; ")) %>%
    
    # Remove duplicates to ensure each (sample, assay) pair is unique
    distinct()
  
  # Combine mean and unique results into a single data frame
  format_raw_data <- left_join(unique_data, mean_data, by = c("sample", "assay"))
  
  # Return the formatted data
  return(format_raw_data)
}


#' @title Preprocess MSD Data
#'
#' @description
#' The `preprocessMSD` function preprocesses MSD (Meso Scale Discovery) data by loading the raw data and standard files,
#' formatting the raw data, extracting relevant standard data, and merging them into a single metadata data frame.
#'
#' @param dataFile A file path to the raw MSD data file (CSV).
#' @param standardFile A file path to the standard curve data file (CSV).
#'
#' @return A data frame containing the merged metadata with formatted raw data and standard information.
#'
#' @examples
#' # Example usage:
#' metadata <- preprocessMSD("path/to/raw_data.csv", "path/to/standard_data.csv")
#'
preprocessMSD <- function(dataFile, standardFile) {
  
  # Load the raw data file, identifying the first row with the keyword "sample"
  raw_data <- read_csv_with_keyword(dataFile, keyword = "sample") 
  
  # Load the standard data file, identifying the first row with the keyword "assay"
  std_data <- read_csv_with_keyword(standardFile, keyword = "assay") 
  
  # Format the raw data using the format_msd_raw_data function
  format_raw_data <- format_msd_raw_data(raw_data)
  
  # Extract relevant variables from the standard data
  format_std_data <- std_data %>%
    mutate(fit_statistic_r_squared = round(as.numeric(fit_statistic_r_squared), 3),
           detection_limits_calc_low = round(as.numeric(detection_limits_calc_low), 3),
           detection_limits_calc_high = round(as.numeric(detection_limits_calc_high), 3)) %>%
    select(all_of(c("assay", "spot", "fit_statistic_r_squared", 
                    "detection_limits_calc_low", "detection_limits_calc_high")))
  
  # Merge the formatted raw data with the formatted standard data based on assay and spot
  metadata <- left_join(format_raw_data, format_std_data, by = c("assay", "spot")) %>%
    arrange(sample)  # Arrange the metadata by sample for easier analysis
  
  # Return the final merged metadata
  return(metadata)
}






# # -------------------------------------------------------------------------
# 
# 
# 
# standardFile <- file.path("/Users/KangDong/Desktop/Lab_app/data/Angiogenesis Panel 1-Plate 1-Standard.csv")
# dataFile <- file.path("/Users/KangDong/Desktop/Lab_app/data/Angiogenesis Panel 1-Plate 1.csv")
# 
# 
# 
# preprocessMSD <- function(dataFile, standardFile) {
#   # Load Data File and Standard File
#   raw_data <- read_csv_with_keyword(dataFile, keyword = "sample") 
#   std_data <- read_csv_with_keyword(standardFile, keyword = "assay") 
# 
#   # Format raw data
#   format_raw_data <- format_msd_raw_data(raw_data)
#   
#   # Report vars for standard 
#   format_std_data <- std_data %>% select(all_of(c("assay", "spot", "fit_statistic_r_squared", "detection_limits_calc_low", "detection_limits_calc_high")))
#   
#   # Join raw and standard
#   metadata <- left_join(format_raw_data, format_std_data, by = c("assay", "spot")) %>%
#     arrange(sample)
#   
#   # Return metadata
#   return(metadata)
# 
# }
# 
# test <- preprocessMSD(dataFile, standardFile)
# 
# 
# 
# 
# 
