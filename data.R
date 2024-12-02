#!/usr/bin/env Rscript

##################
# soccer_data_processing.R
# Soccer Data Preprocessing
##################

# Load required packages
library(tidyverse)

# Data preprocessing function
process_league_data <- function(file_path, league_name) {
  # Read CSV data
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Ensure all required columns are present
  required_cols <- c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR",
                     "HTHG", "HTAG", "HTR", "HS", "AS", "HST", "AST",
                     "HF", "AF", "HC", "AC", "HY", "AY", "HR", "AR")
  
  # Check and report missing columns
  missing_cols <- setdiff(required_cols, names(df))
  if(length(missing_cols) > 0) {
    stop(sprintf("Missing columns in %s: %s", 
                 league_name, 
                 paste(missing_cols, collapse = ", ")))
  }
  
  # Select only the required columns
  df <- df[, required_cols]
  
  # Add league identifier
  df$League <- league_name
  
  # Ensure numeric columns have the correct type
  numeric_cols <- c("FTHG", "FTAG", "HTHG", "HTAG", "HS", "AS", "HST", "AST",
                    "HF", "AF", "HC", "AC", "HY", "AY", "HR", "AR")
  
  for(col in numeric_cols) {
    df[[col]] <- as.numeric(df[[col]])
    # Replace NA with 0
    df[[col]][is.na(df[[col]])] <- 0
  }
  
  # Calculate total shots
  df$TotalShots <- df$HS + df$AS
  
  # Calculate home attack efficiency, avoid division by zero
  df$HomeAttackEff <- ifelse(df$HS > 0, df$HST / df$HS, 0)
  
  # Calculate away attack efficiency, avoid division by zero
  df$AwayAttackEff <- ifelse(df$AS > 0, df$AST / df$AS, 0)
  
  # Calculate home dominance
  total_actions <- df$HS + df$HC + df$AS + df$AC
  df$HomeDominance <- ifelse(total_actions > 0,
                             (df$HS + df$HC) / total_actions,
                             0.5)
  
  # Calculate total goals
  df$TotalGoals <- df$FTHG + df$FTAG
  
  # Calculate goal difference
  df$GoalDiff <- df$FTHG - df$FTAG
  
  # Convert match result to factor
  df$HomeResult <- factor(ifelse(df$GoalDiff > 0, "Win",
                                 ifelse(df$GoalDiff < 0, "Loss", "Draw")),
                          levels = c("Win", "Draw", "Loss"))
  
  return(df)
}

# Main function
main <- function() {
  # Set data file paths
  data_files <- list(
    premier = list(
      path = "~/678-Final-project/premier-league1819.csv",
      name = "Premier League"
    ),
    laliga = list(
      path = "~/678-Final-project/la-ligaseason-1819.csv",
      name = "La Liga"
    ),
    bundesliga = list(
      path = "~/678-Final-project/bundesliga season-1819.csv",
      name = "Bundesliga"
    ),
    seriea = list(
      path = "~/678-Final-project/serie-a1819.csv",
      name = "Serie A"
    ),
    ligue1 = list(
      path = "~/678-Final-project/ligue-1-1819.csv",
      name = "Ligue 1"
    )
  )
  
  # Create empty list to store processed data
  processed_data <- list()
  
  # Process each league's data
  for(league in names(data_files)) {
    tryCatch({
      cat(sprintf("Processing %s data...\n", data_files[[league]]$name))
      
      # Check if file exists
      if(!file.exists(data_files[[league]]$path)) {
        stop(sprintf("File not found: %s", data_files[[league]]$path))
      }
      
      processed_data[[league]] <- process_league_data(
        data_files[[league]]$path,
        data_files[[league]]$name
      )
      
      # Validate processed data structure
      if(ncol(processed_data[[league]]) != 29) {  # Check number of columns
        stop(sprintf("Incorrect number of columns in processed data for %s", 
                     data_files[[league]]$name))
      }
      
    }, error = function(e) {
      cat(sprintf("Error processing %s: %s\n", data_files[[league]]$name, e$message))
      return(NULL)
    })
  }
  
  # Remove failed processing attempts
  processed_data <- processed_data[!sapply(processed_data, is.null)]
  
  if(length(processed_data) == 0) {
    stop("No data was successfully processed")
  }
  
  # Ensure all data frames have the same columns
  first_cols <- names(processed_data[[1]])
  for(league in names(processed_data)) {
    if(!identical(names(processed_data[[league]]), first_cols)) {
      stop(sprintf("Column mismatch in %s", data_files[[league]]$name))
    }
  }
  
  # Merge all league data
  cat("Merging league data...\n")
  all_leagues <- do.call(rbind, processed_data)
  
  # Create output directory
  if(!dir.exists("processed_data")) {
    dir.create("processed_data")
  }
  
  # Save processed data
  cat("Saving processed data...\n")
  saveRDS(all_leagues, "processed_data/all_leagues_processed.rds")
  
  # Save individual league data
  for(league in names(processed_data)) {
    saveRDS(processed_data[[league]], 
            sprintf("processed_data/%s_processed.rds", tolower(league)))
  }
  
  # Output data summary
  cat("\nData Summary:\n")
  cat(sprintf("Total matches: %d\n", nrow(all_leagues)))
  cat("\nMatches by league:\n")
  print(table(all_leagues$League))
  
  # Simple data validation
  cat("\nColumns in processed data:\n")
  print(names(all_leagues))
  
  # Return processed data
  invisible(list(
    all = all_leagues,
    by_league = processed_data
  ))
}

if(interactive()) {
  # Run in RStudio or R console
  main()
} else {
  # Run from command line
  tryCatch({
    main()
  }, error = function(e) {
    cat(sprintf("Critical error in main execution: %s\n", e$message))
    quit(status = 1)
  })
}