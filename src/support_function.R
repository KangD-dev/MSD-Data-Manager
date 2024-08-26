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
    
    # Return the cleaned data as a data frame
    return(data)
  } else {
    stop(paste("Keyword not found in the first", search_limit, "rows of the file."))
  }
}










# -------------------------------------------------------------------------



standardFile <- file.path("/Users/KangDong/Desktop/Lab_app/data/Angiogenesis Panel 1-Plate 1-Standard.csv")
dataFile <- file.path("/Users/KangDong/Desktop/Lab_app/data/Angiogenesis Panel 1-Plate 1.csv")



preprocessMSD <- function(dataFile, standardFile) {
  # Load Data File and Standard File
  raw_data <- read_csv_with_keyword(dataFile, keyword = "sample") %>% clean_names()
  std_data <- read_csv_with_keyword(standardFile, keyword = "assay") %>% clean_names()

  
  # Select columns
  data <- data %>% dplyr::select(SampleID, SubjectID, Plate, Sample, UniqueID, Assay, `Calc. Concentration`, `Calc. Conc. Mean`, `Calc. Conc. CV`, `Detection Limits: Calc. Low`, `Detection Limits: Calc. High` )
  
  
  

  
  
}