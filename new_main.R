# Set log file
install.packages(c(
  "tidyverse", "caret", "glmnet", "brms", "lme4", "MASS", 
  "geepack", "performance", "MatchIt", "nnet", "ggplot2", 
  "gridExtra", "fmsb", "rmarkdown", "dplyr", "tidyr"
))

log_file <- file.path("logs", format(Sys.time(), "modeling_log_%Y%m%d_%H%M%S.txt"))
dir.create("logs", showWarnings = FALSE)

#' Log function
log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(message, "\n", file = log_file, append = TRUE)
  if(level %in% c("ERROR", "WARNING")) {
    cat(message, "\n")
  }
}

#' Load required packages
load_packages <- function() {
  # Set CRAN mirror
  options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
  
  packages <- c(
    "tidyverse", "caret", "glmnet", "brms", "lme4", "MASS", 
    "geepack", "performance", "MatchIt", "nnet", "ggplot2", 
    "gridExtra", "fmsb", "rmarkdown", "dplyr", "tidyr"
  )
  
  for(pkg in packages) {
    tryCatch({
      if(!requireNamespace(pkg, quietly = TRUE)) {
        log_message(sprintf("Installing package: %s", pkg))
        install.packages(pkg)
      }
      library(pkg, character.only = TRUE)
      log_message(sprintf("Successfully loaded package: %s", pkg))
    }, error = function(e) {
      log_message(sprintf("Failed to load package %s: %s", pkg, e$message), "ERROR")
      stop(sprintf("Failed to load package %s", pkg))
    })
  }
}

#' Load and validate data
load_and_validate <- function() {
  # Check if processed data files exist
  data_files <- list(
    all = "processed_data/all_leagues_processed.rds",
    bundesliga = "processed_data/bundesliga_processed.rds",
    laliga = "processed_data/laliga_processed.rds", 
    ligue1 = "processed_data/ligue1_processed.rds",
    premier = "processed_data/premier_processed.rds",
    seriea = "processed_data/seriea_processed.rds"
  )
  
  data_list <- list()
  
  for(name in names(data_files)) {
    file_path <- data_files[[name]]
    tryCatch({
      if(file.exists(file_path)) {
        data_list[[name]] <- readRDS(file_path)
        log_message(sprintf("Successfully loaded data for %s", name))
      } else {
        log_message(sprintf("File not found: %s", file_path), "WARNING")
      }
    }, error = function(e) {
      log_message(sprintf("Error loading %s: %s", name, e$message), "ERROR")
    })
  }
  
  if(length(data_list) == 0) {
    stop("No data files could be loaded")
  }
  
  return(data_list)
}

#' No Polling Models
fit_no_polling <- function(data, leagues) {
  models <- list()
  
  # Check if required variables exist
  required_vars <- c("League", "TotalGoals", "HomeResult", "GoalDiff",
                     "HS", "AS", "HST", "AST", "HF", "AF", "HC", "AC",
                     "HomeAttackEff", "AwayAttackEff", "HomeDominance")
  
  missing_vars <- setdiff(required_vars, names(data))
  if(length(missing_vars) > 0) {
    stop(sprintf("Missing required variables: %s", 
                 paste(missing_vars, collapse = ", ")))
  }
  
  for(league in leagues) {
    log_message(sprintf("Fitting models for league: %s", league))
    
    league_data <- data %>% 
      filter(League == league)
    
    if(nrow(league_data) == 0) {
      log_message(sprintf("No data available for league: %s", league), "WARNING")
      next
    }
    
    tryCatch({
      # 1. Total goals prediction (using Poisson regression)
      goals_formula <- TotalGoals ~ HS + AS + HST + AST + HF + AF + HC + AC +
        HomeAttackEff + AwayAttackEff + HomeDominance
      
      goals_model <- glm(goals_formula,
                         family = poisson(link = "log"),
                         data = league_data,
                         control = list(maxit = 100))
      
      # 2. Match result prediction (using multinomial logistic regression)
      result_formula <- HomeResult ~ HS + AS + HST + AST + HF + AF + HC + AC +
        HomeAttackEff + AwayAttackEff + HomeDominance
      
      result_model <- multinom(result_formula,
                               data = league_data,
                               trace = FALSE,
                               maxit = 1000)
      
      # 3. Goal difference prediction (using linear regression)
      diff_formula <- GoalDiff ~ HS + AS + HST + AST + HF + AF + HC + AC +
        HomeAttackEff + AwayAttackEff + HomeDominance
      
      diff_model <- lm(diff_formula, data = league_data)
      
      models[[league]] <- list(
        goals = goals_model,
        result = result_model,
        diff = diff_model
      )
      
      log_message(sprintf("Successfully fitted models for league: %s", league))
      
    }, error = function(e) {
      log_message(sprintf("Error in model fitting for league %s: %s", 
                          league, e$message), "ERROR")
      return(NULL)
    })
  }
  
  if(length(models) == 0) {
    log_message("No models could be fitted", "WARNING")
  }
  
  return(models)
}

#' Partial Polling Models
fit_partial_polling <- function(data) {
  log_message("Starting partial polling models fitting")
  
  # Data preprocessing
  data <- data %>%
    group_by(League) %>%
    mutate(
      AttackingStyle = mean(TotalGoals, na.rm = TRUE) > median(TotalGoals, na.rm = TRUE),
      LeagueStrength = mean(TotalShots, na.rm = TRUE) > median(TotalShots, na.rm = TRUE),
      LeagueType = factor(paste0(
        ifelse(AttackingStyle, "High", "Low"), "_",
        ifelse(LeagueStrength, "Strong", "Weak")
      )),
      match_id = row_number(),
      time_index = match_id
    ) %>%
    ungroup()
  
  tryCatch({
    # 1. Multilevel Poisson regression (goals)
    log_message("Fitting multilevel Poisson regression...")
    mlm_goals <- glmer(
      TotalGoals ~ HS + AS + (1|League),
      family = poisson(link = "log"),
      data = data,
      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
    )
    
    # 2. Multilevel multinomial regression (match result)
    log_message("Fitting multilevel multinomial regression...")
    mlm_result <- multinom(
      HomeResult ~ HS + AS + League,
      data = data,
      trace = FALSE,
      MaxNWts = 5000
    )
    
    # 3. GEE model (considering time dependency)
    log_message("Fitting GEE model...")
    data$id <- as.numeric(factor(data$League))
    data$wave <- as.numeric(factor(data$match_id))
    
    gee_model <- geeglm(
      TotalGoals ~ HS + AS,
      id = id,
      wave = wave,
      data = data,
      family = poisson(link = "log"),
      corstr = "exchangeable"
    )
    
    log_message("Successfully fitted all partial polling models")
    
    return(list(
      mlm_goals = mlm_goals,
      mlm_result = mlm_result,
      gee = gee_model
    ))
    
  }, error = function(e) {
    log_message(sprintf("Error in partial polling models: %s", e$message), "ERROR")
    return(NULL)
  })
}

#' Complete Polling Models
fit_complete_polling <- function(data) {
  log_message("Starting complete polling models fitting")
  
  tryCatch({
    # 1. Poisson regression (total goals)
    log_message("Fitting Poisson regression...")
    complete_goals <- glm(
      TotalGoals ~ HS + AS + HST + AST + League,
      family = poisson(),
      data = data,
      control = list(maxit = 1000)
    )
    
    # 2. Multinomial regression (match result)
    log_message("Fitting multinomial regression...")
    complete_result <- multinom(
      HomeResult ~ HS + AS + HST + AST + League,
      data = data,
      trace = FALSE,
      MaxNWts = 5000
    )
    
    # 3. Bayesian multilevel model
    log_message("Fitting Bayesian multilevel model...")
    bayes_model <- brm(
      formula = TotalGoals ~ HS + AS + HST + AST + (1|League),
      data = data,
      family = poisson(),
      prior = c(
        prior(normal(0, 5), class = "b"),
        prior(normal(0, 2), class = "Intercept"),
        prior(student_t(3, 0, 2), class = "sd")
      ),
      chains = 4,
      iter = 2000,
      warmup = 1000,
      cores = 4,
      control = list(adapt_delta = 0.99, max_treedepth = 15)
    )
    
    log_message("Successfully fitted all complete polling models")
    
    return(list(
      goals = complete_goals,
      result = complete_result,
      bayes = bayes_model
    ))
    
  }, error = function(e) {
    log_message(sprintf("Error in complete polling models: %s", e$message), "ERROR")
    return(NULL)
  })
}

#' Calculate Kappa coefficient
calculate_kappa <- function(actual, predicted) {
  # Create confusion matrix
  conf_matrix <- table(actual, predicted)
  
  # Calculate observed agreement
  observed_agreement <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  # Calculate expected agreement
  row_sums <- rowSums(conf_matrix)
  col_sums <- colSums(conf_matrix)
  expected_agreement <- sum(row_sums * col_sums) / (sum(conf_matrix)^2)
  
  # Calculate Kappa
  kappa <- (observed_agreement - expected_agreement) / (1 - expected_agreement)
  
  return(kappa)
}

#' Model evaluation
evaluate_models <- function(no_polling, partial_polling, complete_polling, data) {
  results <- list()
  
  # 1. Evaluate No Polling models
  log_message("Evaluating No Polling models...")
  no_polling_metrics <- list()
  
  for(league in names(no_polling)) {
    if(!is.null(no_polling[[league]])) {
      metrics <- list()
      tryCatch({
        # Goals prediction evaluation
        pred_goals <- predict(no_polling[[league]]$goals, newdata = data, type = "response")
        metrics$goals <- c(
          RMSE = sqrt(mean((data$TotalGoals - pred_goals)^2, na.rm = TRUE)),
          MAE = mean(abs(data$TotalGoals - pred_goals), na.rm = TRUE)
        )
        
        # Match result prediction evaluation
        pred_result <- predict(no_polling[[league]]$result, newdata = data)
        metrics$result <- c(
          Accuracy = mean(pred_result == data$HomeResult, na.rm = TRUE),
          Kappa = calculate_kappa(data$HomeResult, pred_result)
        )
        
        no_polling_metrics[[league]] <- metrics
        
      }, error = function(e) {
        log_message(sprintf("Error in evaluating no polling model for %s: %s", league, e$message), "WARNING")
      })
    }
  }
  
  results$no_polling <- no_polling_metrics
  
  # 2. Evaluate Partial Polling models
  if(!is.null(partial_polling)) {
    partial_metrics <- list()
    
    tryCatch({
      if(!is.null(partial_polling$mlm_goals)) {
        pred_goals <- predict(partial_polling$mlm_goals, newdata = data, 
                              re.form = NULL)
        partial_metrics$goals <- c(
          RMSE = sqrt(mean((data$TotalGoals - pred_goals)^2, na.rm = TRUE)),
          MAE = mean(abs(data$TotalGoals - pred_goals), na.rm = TRUE)
        )
      }
      
      if(!is.null(partial_polling$mlm_result)) {
        pred_result <- predict(partial_polling$mlm_result, newdata = data)
        partial_metrics$result <- c(
          Accuracy = mean(pred_result == data$HomeResult, na.rm = TRUE),
          Kappa = calculate_kappa(data$HomeResult, pred_result)
        )
      }
      
      results$partial_polling <- partial_metrics
      
    }, error = function(e) {
      log_message(sprintf("Error in partial polling evaluation: %s", e$message), "WARNING")
    })
  }
  
  # 3. Evaluate Complete Polling models
  if(!is.null(complete_polling)) {
    complete_metrics <- list()
    
    tryCatch({
      # Evaluate Poisson regression
      if(!is.null(complete_polling$goals)) {
        pred_goals <- predict(complete_polling$goals, newdata = data, 
                              type = "response")
        complete_metrics$goals <- c(
          RMSE = sqrt(mean((data$TotalGoals - pred_goals)^2, na.rm = TRUE)),
          MAE = mean(abs(data$TotalGoals - pred_goals), na.rm = TRUE)
        )
      }
      
      # Evaluate multinomial regression
      if(!is.null(complete_polling$result)) {
        pred_result <- predict(complete_polling$result, newdata = data)
        complete_metrics$result <- c(
          Accuracy = mean(pred_result == data$HomeResult, na.rm = TRUE),
          Kappa = calculate_kappa(data$HomeResult, pred_result)
        )
      }
      
      # Evaluate Bayesian model
      if(!is.null(complete_polling$bayes)) {
        pred_bayes <- fitted(complete_polling$bayes)  # Use fitted() instead of predict()
        complete_metrics$bayes <- c(
          RMSE = sqrt(mean((data$TotalGoals - pred_bayes[,1])^2, na.rm = TRUE)),
          MAE = mean(abs(data$TotalGoals - pred_bayes[,1]), na.rm = TRUE)
        )
      }
      
      results$complete_polling <- complete_metrics
      
    }, error = function(e) {
      log_message(sprintf("Error in complete polling evaluation: %s", e$message), "WARNING")
    })
  }
  
  return(results)
}

#' Create results summary
create_summary <- function(eval_results) {
  # Create an empty data frame
  summary_df <- data.frame(
    model_type = character(),
    goals_rmse = numeric(),
    goals_mae = numeric(),
    result_accuracy = numeric(),
    result_kappa = numeric(),
    bayes_rmse = numeric(),
    bayes_mae = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process No Polling results
  if(!is.null(eval_results$no_polling)) {
    goals_metrics <- sapply(eval_results$no_polling, function(x) x$goals)
    result_metrics <- sapply(eval_results$no_polling, function(x) x$result)
    
    no_polling_summary <- data.frame(
      model_type = "no_polling",
      goals_rmse = mean(unlist(goals_metrics["RMSE",]), na.rm = TRUE),
      goals_mae = mean(unlist(goals_metrics["MAE",]), na.rm = TRUE),
      result_accuracy = mean(unlist(result_metrics["Accuracy",]), na.rm = TRUE),
      result_kappa = mean(unlist(result_metrics["Kappa",]), na.rm = TRUE),
      bayes_rmse = NA,
      bayes_mae = NA
    )
    summary_df <- rbind(summary_df, no_polling_summary)
  }
  
  # Process Partial Polling results
  if(!is.null(eval_results$partial_polling)) {
    partial_polling_summary <- data.frame(
      model_type = "partial_polling",
      goals_rmse = eval_results$partial_polling$goals["RMSE"],
      goals_mae = eval_results$partial_polling$goals["MAE"],
      result_accuracy = eval_results$partial_polling$result["Accuracy"],
      result_kappa = eval_results$partial_polling$result["Kappa"],
      bayes_rmse = NA,
      bayes_mae = NA
    )
    summary_df <- rbind(summary_df, partial_polling_summary)
  }
  
  # Process Complete Polling results
  if(!is.null(eval_results$complete_polling)) {
    complete_polling_summary <- data.frame(
      model_type = "complete_polling",
      goals_rmse = eval_results$complete_polling$goals["RMSE"],
      goals_mae = eval_results$complete_polling$goals["MAE"],
      result_accuracy = eval_results$complete_polling$result["Accuracy"],
      result_kappa = eval_results$complete_polling$result["Kappa"],
      bayes_rmse = eval_results$complete_polling$bayes["RMSE"],
      bayes_mae = eval_results$complete_polling$bayes["MAE"]
    )
    summary_df <- rbind(summary_df, complete_polling_summary)
  }
  
  rownames(summary_df) <- summary_df$model_type
  summary_df$model_type <- NULL
  
  return(summary_df)
}

#' Create league comparison analysis
create_league_comparison <- function(data) {
  # Create directory for saving figures
  if(!dir.exists("analysis_results/figures")) {
    dir.create("analysis_results/figures", recursive = TRUE)
  }
  
  # 1. Goals distribution analysis
  goals_plot <- ggplot(data, aes(x = League, y = TotalGoals, fill = League)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Goals Distribution Comparison",
         x = "League",
         y = "Total Goals") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("analysis_results/figures/goals_distribution.png", goals_plot, width = 10, height = 6)
  
  # 2. Home advantage analysis
  home_advantage <- data %>%
    group_by(League) %>%
    summarise(
      home_win_rate = mean(HomeResult == "Win", na.rm = TRUE),
      home_goals_per_game = mean(FTHG, na.rm = TRUE),
      away_goals_per_game = mean(FTAG, na.rm = TRUE)
    )
  
  home_adv_plot <- ggplot(home_advantage, aes(x = League)) +
    geom_col(aes(y = home_win_rate, fill = "Home Win Rate")) +
    geom_point(aes(y = home_goals_per_game, color = "Home Goals per Game")) +
    geom_point(aes(y = away_goals_per_game, color = "Away Goals per Game")) +
    theme_minimal() +
    labs(title = "Home Advantage Analysis",
         x = "League",
         y = "Rate") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("analysis_results/figures/home_advantage.png", home_adv_plot, width = 10, height = 6)
  
  # 3. Attacking and defending radar chart
  league_stats <- data %>%
    group_by(League) %>%
    summarise(
      avg_goals = mean(TotalGoals, na.rm = TRUE),
      shots_accuracy = mean(HST/HS, na.rm = TRUE),
      shot_conversion = mean(FTHG/HST, na.rm = TRUE),
      possession = mean(HomeDominance, na.rm = TRUE),
      corner_kicks = mean(HC, na.rm = TRUE)
    )
  
  radar_data <- as.data.frame(t(league_stats[,-1]))
  colnames(radar_data) <- league_stats$League
  radar_data <- rbind(apply(radar_data, 2, max),
                      apply(radar_data, 2, min),
                      radar_data)
  
  png("analysis_results/figures/radar_chart.png", width = 800, height = 800)
  radarchart(radar_data,
             pcol = rainbow(ncol(radar_data)),
             title = "League Attacking and Defending Comparison")
  legend("topright",
         legend = colnames(radar_data),
         col = rainbow(ncol(radar_data)),
         lty = 1)
  dev.off()
  
  # 4. Team strength distribution
  team_strength <- data %>%
    group_by(League, HomeTeam) %>%
    summarise(
      avg_goals_scored = mean(FTHG, na.rm = TRUE),
      avg_goals_conceded = mean(FTAG, na.rm = TRUE),
      points = sum(case_when(
        HomeResult == "Win" ~ 3,
        HomeResult == "Draw" ~ 1,
        TRUE ~ 0
      )),
      .groups = 'drop'
    )
  
  strength_plot <- ggplot(team_strength, 
                          aes(x = avg_goals_scored, 
                              y = avg_goals_conceded, 
                              color = League)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Team Strength Distribution",
         x = "Goals per Game",
         y = "Goals Conceded per Game")
  
  ggsave("analysis_results/figures/team_strength.png", strength_plot, width = 10, height = 6)
  
  return(list(
    goals_stats = summary(data$TotalGoals),
    home_advantage = home_advantage,
    league_stats = league_stats,
    team_strength = team_strength
  ))
}

#' Create temporal trend analysis
create_temporal_analysis <- function(data) {
  # Ensure date format is correct
  data$Date <- as.Date(data$Date)
  
  # 1. Goals trend by month
  monthly_goals <- data %>%
    mutate(Month = format(Date, "%Y-%m")) %>%
    group_by(League, Month) %>%
    summarise(
      avg_goals = mean(TotalGoals, na.rm = TRUE),
      .groups = 'drop'
    )
  
  goals_trend_plot <- ggplot(monthly_goals, 
                             aes(x = Month, y = avg_goals, 
                                 color = League, group = League)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Monthly Goals Trend",
         x = "Month",
         y = "Average Goals") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("analysis_results/figures/monthly_goals_trend.png", 
         goals_trend_plot, width = 12, height = 6)
  
  # 2. Season fatigue analysis
  fatigue_analysis <- data %>%
    mutate(Month = format(Date, "%m")) %>%
    group_by(League, Month) %>%
    summarise(
      fouls_per_game = mean(HF + AF, na.rm = TRUE),
      cards_per_game = mean(HY + AY + HR + AR, na.rm = TRUE),
      goals_per_game = mean(TotalGoals, na.rm = TRUE),
      shots_per_game = mean(HS + AS, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Plot comprehensive fatigue metrics
  # Create fatigue trend visualization
  log_message("Creating fatigue trend visualization...")
  
  # Data validation  
  required_cols <- c("Date", "HY", "AY", "HR", "AR", "HF", "AF")
  if(!all(required_cols %in% names(data))) {
    log_message("Missing required columns for fatigue trend analysis", "WARNING")
    log_message(sprintf("Missing: %s", 
                        paste(setdiff(required_cols, names(data)), collapse = ", ")))
  } else {
    fatigue_trend <- data %>%
      mutate(Date = as.Date(Date)) %>%
      group_by(League, Month = format(Date, "%m")) %>%
      summarise(
        cards_per_game = mean(HY + AY + HR + AR, na.rm = TRUE),
        fouls_per_game = mean(HF + AF, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        fatigue_index = scale(cards_per_game + fouls_per_game),
        Month = factor(Month, levels = sprintf("%02d", 1:12))
      )
    
    fatigue_plot <- ggplot(fatigue_trend, 
                           aes(x = Month, y = fatigue_index, 
                               color = League, group = League)) +
      geom_line() +
      geom_point() +
      facet_wrap(~League) +
      labs(title = "Season Fatigue Index Trend",
           x = "Month", 
           y = "Fatigue Index") +
      theme_minimal()
    
    ggsave("analysis_results/figures/fatigue_trends.png", 
           fatigue_plot, width = 15, height = 10)
  }
  
  return(list(
    monthly_goals = monthly_goals,
    fatigue_analysis = fatigue_analysis
  ))
}

#' Generate detailed Markdown report
generate_markdown_report <- function(analysis_results, data) {
  # Calculate some basic statistics
  league_summaries <- data %>%
    group_by(League) %>%
    summarise(
      total_matches = n(),
      total_goals = sum(TotalGoals),
      avg_goals = mean(TotalGoals, na.rm = TRUE),
      home_win_rate = mean(HomeResult == "Win", na.rm = TRUE),
      draw_rate = mean(HomeResult == "Draw", na.rm = TRUE),
      away_win_rate = mean(HomeResult == "Loss", na.rm = TRUE),
      avg_shots = mean(HS + AS, na.rm = TRUE),
      avg_shots_on_target = mean(HST + AST, na.rm = TRUE),
      avg_corners = mean(HC + AC, na.rm = TRUE),
      avg_fouls = mean(HF + AF, na.rm = TRUE),
      avg_cards = mean(HY + AY + HR + AR, na.rm = TRUE)
    )
  
  # Match pattern analysis
  match_patterns <- data %>%
    mutate(
      high_scoring = TotalGoals >= 4,
      low_scoring = TotalGoals <= 1,
      clean_sheet = FTHG == 0 | FTAG == 0,
      comeback_win = (HTHG < HTAG & FTR == "H") | (HTHG > HTAG & FTR == "A")
    ) %>%
    group_by(League) %>%
    summarise(
      high_scoring_rate = mean(high_scoring, na.rm = TRUE),
      low_scoring_rate = mean(low_scoring, na.rm = TRUE),
      clean_sheet_rate = mean(clean_sheet, na.rm = TRUE),
      comeback_rate = mean(comeback_win, na.rm = TRUE)
    )
  
  report <- c(
    "# In-Depth Analysis Report of Top 5 Soccer Leagues\n\n",
    
    "## Table of Contents\n",
    "1. [Overall League Overview](#overall-league-overview)\n",
    "2. [Goals Analysis](#goals-analysis)\n",
    "3. [Home Advantage Analysis](#home-advantage-analysis)\n",
    "4. [Match Pattern Analysis](#match-pattern-analysis)\n",
    "5. [Technical Metrics Analysis](#technical-metrics-analysis)\n",
    "6. [League Feature Comparison](#league-feature-comparison)\n",
    "7. [Temporal Trend Analysis](#temporal-trend-analysis)\n",
    "8. [Special Match Patterns](#special-match-patterns)\n",
    "9. [Summary and Insights](#summary-and-insights)\n\n",
    
    "## Overall League Overview\n\n",
    
    paste(sapply(1:nrow(league_summaries), function(i) {
      league <- league_summaries[i,]
      sprintf(
        "### %s\n\n- Total Matches: %d\n- Total Goals: %d\n- Average Goals per Match: %.2f\n- Home Win Rate: %.1f%%\n- Draw Rate: %.1f%%\n- Away Win Rate: %.1f%%\n\n",
        league$League,
        league$total_matches,
        league$total_goals,
        league$avg_goals,
        league$home_win_rate * 100,
        league$draw_rate * 100,
        league$away_win_rate * 100
      )
    }), collapse = "\n"),
    
    "## Goals Analysis\n\n",
    
    "### Goals Distribution\n\n",
    "![Goals Distribution](figures/goals_distribution.png)\n\n",
    
    paste(sapply(1:nrow(league_summaries), function(i) {
      league <- league_summaries[i,]
      sprintf(
        "- %s's Goal Characteristics:\n  - Average %.2f goals per match\n  - %.1f%% of matches are high-scoring (4+ goals)\n  - %.1f%% of matches are low-scoring (0-1 goals)\n",
        league$League,
        league$avg_goals,
        match_patterns[i,]$high_scoring_rate * 100,
        match_patterns[i,]$low_scoring_rate * 100
      )
    }), collapse = "\n"),
    
    "\n## Home Advantage Analysis\n\n",
    "![Home Advantage](figures/home_advantage.png)\n\n",
    
    paste(sapply(1:nrow(league_summaries), function(i) {
      league <- league_summaries[i,]
      sprintf(
        "- %s's Home Advantage:\n  - Home Win Rate: %.1f%%\n  - Home Goal Efficiency: %.2f per match\n  - Home Possession Rate: %.1f%%\n",
        league$League,
        league$home_win_rate * 100,
        league$avg_goals,
        mean(data$HomeDominance[data$League == league$League], na.rm = TRUE) * 100
      )
    }), collapse = "\n"),
    
    "\n## Technical Metrics Analysis\n\n",
    "![Attacking and Defending](figures/radar_chart.png)\n\n",
    
    paste(sapply(1:nrow(league_summaries), function(i) {
      league <- league_summaries[i,]
      sprintf(
        "- %s's Technical Characteristics:\n  - Average Shots per Match: %.1f\n  - Shots on Target Rate: %.1f%%\n  - Corners: %.1f per match\n  - Fouls: %.1f per match\n",
        league$League,
        league$avg_shots,
        (league$avg_shots_on_target / league$avg_shots) * 100,
        league$avg_corners,
        league$avg_fouls
      )
    }), collapse = "\n"),
    
    "\n## Temporal Trend Analysis\n\n",
    "### Changes During the Season\n",
    "![Monthly Goals Trend](figures/monthly_goals_trend.png)\n\n",
    "![Fatigue Metrics](figures/fatigue_trends.png)\n\n",
    
    "### Season Fatigue Analysis\n",
    sprintf(
      "- Highest Season Stage for Fouls: %s\n",
      names(which.max(tapply(data$HF + data$AF, 
                             format(as.Date(data$Date), "%B"), 
                             mean, na.rm = TRUE)))
    ),
    sprintf(
      "- Highest Season Stage for Goals: %s\n",
      names(which.max(tapply(data$TotalGoals, 
                             format(as.Date(data$Date), "%B"), 
                             mean, na.rm = TRUE)))
    ),
    
    "\n## Special Match Patterns\n\n",
    paste(sapply(1:nrow(match_patterns), function(i) {
      pattern <- match_patterns[i,]
      sprintf(
        "- %s:\n  - Clean Sheet Rate: %.1f%%\n  - High Score Rate: %.1f%%\n  - Comeback Rate: %.1f%%\n",
        league_summaries$League[i],
        pattern$clean_sheet_rate * 100,
        pattern$high_scoring_rate * 100,
        pattern$comeback_rate * 100
      )
    }), collapse = "\n"),
    
    "\n## Summary and Insights\n\n",
    
    "### Summary of League Characteristics\n",
    paste(sapply(unique(data$League), function(league) {
      league_data <- data[data$League == league,]
      sprintf(
        "- %s:\n  - Characteristics: %s\n  - Strengths: %s\n  - Challenges: %s\n",
        league,
        ifelse(mean(league_data$TotalGoals) > mean(data$TotalGoals), "Offensive League", "Defensive League"),
        ifelse(mean(league_data$HomeResult == "Win") > mean(data$HomeResult == "Win"), "Strong Home Advantage", "Balanced Home and Away"),
        ifelse(sd(league_data$TotalGoals) > sd(data$TotalGoals), "Unstable Match Outcomes", "Stable Match Outcomes")
      )
    }), collapse = "\n"),
    
    "\n### Key Findings\n",
    "1. Goal Characteristics:\n",
    sprintf("   - League with Highest Goal Efficiency: %s\n",
            league_summaries$League[which.max(league_summaries$avg_goals)]),
    sprintf("   - Most Stable League: %s\n",
            league_summaries$League[which.min(sapply(split(data$TotalGoals, data$League), sd, na.rm = TRUE))]),
    "2. Match Patterns:\n",
    sprintf("   - Most Entertaining League: %s (Based on High Goal Rate and Shots)\n",
            league_summaries$League[which.max(league_summaries$avg_shots)]),
    sprintf("   - Most Intense League: %s (Based on Fouls and Cards)\n",
            league_summaries$League[which.max(league_summaries$avg_fouls)]),
    "3. Special Findings:\n",
    sprintf("   - League with Highest Comeback Rate: %s\n",
            match_patterns$League[which.max(match_patterns$comeback_rate)]),
    sprintf("   - League with Highest Clean Sheet Rate: %s\n",
            match_patterns$League[which.max(match_patterns$clean_sheet_rate)]),
    
    "\n### Recommendations and Outlook\n",
    "1. League Development Suggestions:\n",
    "   - Strengthen offensive tactics\n",
    "   - Balance home and away advantages\n",
    "   - Enhance match entertainment value\n",
    "2. Data Application Suggestions:\n",
    "   - Analyze team tactical characteristics in depth\n",
    "   - Optimize schedule arrangements\n",
    "   - Improve refereeing standards\n",
    "3. Future Research Directions:\n",
    "   - Player performance data analysis\n",
    "   - Tactical trend research\n",
    "   - Cross-season comparative study\n"
  )
  
  # Write to markdown file
  writeLines(report, "analysis_results/analysis_report.md")
  
  # Generate HTML version (if rmarkdown package is installed)
  if(requireNamespace("rmarkdown", quietly = TRUE)) {
    tryCatch({
      rmarkdown::render("analysis_results/analysis_report.md",
                        output_format = "html_document",
                        output_file = "analysis_report.html")
    }, error = function(e) {
      warning("HTML report generation failed: ", e$message)
    })
  }
}

#' Main function
main <- function() {
  # Set random seed for reproducibility
  set.seed(123)
  
  # Create necessary directories
  dirs <- c("logs", "model_results", "analysis_results", "analysis_results/figures")
  for(dir in dirs) {
    if(!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      log_message(sprintf("Created directory: %s", dir))
    }
  }
  
  # Load required packages
  load_packages()
  
  # Load data
  tryCatch({
    log_message("Loading and validating data...")
    data_list <- load_and_validate()
    
    # Check if data was successfully loaded
    if(is.null(data_list$all)) {
      stop("Failed to load all leagues data")
    }
    
    # Get list of leagues
    leagues <- unique(data_list$all$League)
    
    if(length(leagues) == 0) {
      stop("No league data available")
    }
    
    # === Modeling Analysis ===
    
    # 1. No Polling Models
    log_message("Fitting No Polling models...")
    no_polling_models <- fit_no_polling(data_list$all, leagues)
    
    # 2. Partial Polling Models
    log_message("Fitting Partial Polling models...")
    partial_polling_models <- fit_partial_polling(data_list$all)
    
    # 3. Complete Polling Models
    log_message("Fitting Complete Polling models...")
    complete_polling_models <- fit_complete_polling(data_list$all)
    
    # Evaluate models
    log_message("Evaluating models...")
    eval_results <- evaluate_models(
      no_polling_models,
      partial_polling_models,
      complete_polling_models,
      data_list$all
    )
    
    # Create results summary
    log_message("Creating results summary...")
    summary_results <- create_summary(eval_results)
    
    # === Multi-Dimensional Analysis ===
    
    log_message("Starting comprehensive analysis...")
    
    # Calculate league summary statistics
    league_summaries <- data_list$all %>%
      group_by(League) %>%
      summarise(
        total_matches = n(),
        total_goals = sum(TotalGoals),
        avg_goals = mean(TotalGoals, na.rm = TRUE),
        home_win_rate = mean(HomeResult == "Win", na.rm = TRUE),
        draw_rate = mean(HomeResult == "Draw", na.rm = TRUE),
        away_win_rate = mean(HomeResult == "Loss", na.rm = TRUE),
        avg_shots = mean(HS + AS, na.rm = TRUE),
        avg_shots_on_target = mean(HST + AST, na.rm = TRUE),
        avg_corners = mean(HC + AC, na.rm = TRUE),
        avg_fouls = mean(HF + AF, na.rm = TRUE),
        avg_cards = mean(HY + AY + HR + AR, na.rm = TRUE)
      )
    
    # Calculate match pattern statistics
    match_patterns <- data_list$all %>%
      mutate(
        high_scoring = TotalGoals >= 4,
        low_scoring = TotalGoals <= 1,
        clean_sheet = FTHG == 0 | FTAG == 0,
        comeback_win = (HTHG < HTAG & FTR == "H") | (HTHG > HTAG & FTR == "A")
      ) %>%
      group_by(League) %>%
      summarise(
        high_scoring_rate = mean(high_scoring, na.rm = TRUE),
        low_scoring_rate = mean(low_scoring, na.rm = TRUE),
        clean_sheet_rate = mean(clean_sheet, na.rm = TRUE),
        comeback_rate = mean(comeback_win, na.rm = TRUE)
      )
    
    # 1. League Comparison Analysis
    log_message("Performing league comparison analysis...")
    comparison_results <- create_league_comparison(data_list$all)
    
    # 2. Temporal Trend Analysis
    log_message("Performing temporal analysis...")
    temporal_results <- create_temporal_analysis(data_list$all)
    
    # === Summary of Results and Report Generation ===
    
    # 1. Generate analysis report
    log_message("Generating analysis report...")
    analysis_results <- list(
      comparison = comparison_results,
      temporal = temporal_results,
      league_summaries = league_summaries,
      match_patterns = match_patterns
    )
    generate_markdown_report(analysis_results, data_list$all)
    
    # 2. Save analysis results
    log_message("Saving analysis results...")
    saveRDS(analysis_results, "analysis_results/analysis_results.rds")
    
    # 3. Save model results
    log_message("Saving modeling results...")
    saveRDS(list(
      no_polling = no_polling_models,
      partial_polling = partial_polling_models,
      complete_polling = complete_polling_models
    ), "model_results/model_objects.rds")
    
    # 4. Save evaluation results
    saveRDS(eval_results, "model_results/evaluation_results.rds")
    write.csv(summary_results, "model_results/model_summary.csv")
    
    log_message("Analysis completed successfully")
    
  }, error = function(e) {
    log_message(sprintf("Error in main execution: %s", e$message), "ERROR")
    stop("Analysis failed")
  })
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
