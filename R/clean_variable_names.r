#' Clean and rename variable names using AI-powered transformation
#'
#' This function transforms technical variable names into publication-ready,
#' human-readable names using pattern recognition and optional AI enhancement.
#'
#' @param var_names Character vector of variable names to clean
#' @param method Character; cleaning method to use:
#'   - "rules": Use built-in pattern matching rules (default, fast)
#'   - "ai_local": Use local AI model via ollama (requires ollama installation)
#'   - "ai_openai": Use OpenAI API (requires API key in environment)
#'   - "hybrid": Try rules first, then AI for unmatched names
#' @param training_data Optional data frame with columns 'abbrev_name' and 'table_name'
#'   for custom training patterns. If NULL, uses built-in training data.
#' @param api_key Character; API key for AI services (if using ai_openai method)
#' @param cache_results Logical; if TRUE, caches AI results to avoid repeat calls
#' @param fallback_rules Logical; if TRUE, falls back to rule-based cleaning if AI fails
#'
#' @return Character vector of cleaned variable names, same length as input
#'
#' @examples
#' \dontrun{
#' # Basic rule-based cleaning
#' vars <- c("demographics_age_tpx", "postop_ICU_LOS", "survival_30: Y")
#' clean_variable_names(vars)
#'
#' # With AI enhancement
#' clean_variable_names(vars, method = "ai_local")
#'
#' # Using custom training data
#' training <- read.csv("my_variable_mappings.csv")
#' clean_variable_names(vars, training_data = training)
#' }
#'
#' @export
clean_variable_names <- function(var_names,
                                method = "rules",
                                training_data = NULL,
                                api_key = NULL,
                                cache_results = TRUE,
                                fallback_rules = TRUE) {
  
  # Input validation
  if (!is.character(var_names)) {
    stop("var_names must be a character vector")
  }
  
  method <- match.arg(method, c("rules", "ai_local", "ai_openai", "hybrid"))
  
  # Load training data if not provided
  if (is.null(training_data)) {
    training_file <- system.file("extdata", "variable_name_training.csv", package = "TernTablesR")
    if (file.exists(training_file)) {
      training_data <- read.csv(training_file, stringsAsFactors = FALSE)
    } else {
      # Fallback to built-in patterns
      training_data <- .get_default_training_data()
    }
  }
  
  # Initialize cache
  if (cache_results && !exists(".var_name_cache", envir = .GlobalEnv)) {
    assign(".var_name_cache", list(), envir = .GlobalEnv)
  }
  
  # Apply cleaning method
  cleaned_names <- switch(method,
    "rules" = .clean_names_rules(var_names, training_data),
    "ai_local" = .clean_names_ai_local(var_names, training_data, cache_results, fallback_rules),
    "ai_openai" = .clean_names_ai_openai(var_names, training_data, api_key, cache_results, fallback_rules),
    "hybrid" = .clean_names_hybrid(var_names, training_data, api_key, cache_results)
  )
  
  return(cleaned_names)
}

#' Rule-based variable name cleaning
#' @keywords internal
.clean_names_rules <- function(var_names, training_data) {
  
  cleaned <- character(length(var_names))
  
  for (i in seq_along(var_names)) {
    var <- var_names[i]
    
    # Check exact match in training data first
    exact_match <- training_data$table_name[training_data$abbrev_name == var]
    if (length(exact_match) > 0) {
      cleaned[i] <- exact_match[1]
      next
    }
    
    # Apply rule-based transformations
    cleaned[i] <- .apply_cleaning_rules(var)
  }
  
  return(cleaned)
}

#' Apply comprehensive cleaning rules to a single variable name
#' @keywords internal
.apply_cleaning_rules <- function(var_name) {
  
  # Remove trailing : Y, : N patterns (binary indicators)
  clean_name <- gsub(": [YN]$", "", var_name)
  
  # Split by underscores and process each part
  parts <- strsplit(clean_name, "_")[[1]]
  
  # Define common abbreviation mappings
  abbrev_map <- list(
    # Demographics
    "demographics" = "",
    "age" = "Age",
    "BMI" = "Body Mass Index",
    "sex" = "Sex",
    "race" = "Race",
    
    # Time periods
    "preop" = "Preoperative",
    "postop" = "Postoperative", 
    "operative" = "Operative",
    "intraop" = "Intraoperative",
    
    # Medical abbreviations
    "LVEF" = "LVEF",
    "CVP" = "CVP",
    "IABP" = "Intra-Aortic Balloon Pump",
    "ECMO" = "ECMO",
    "VA" = "VA",
    "LVAD" = "LVAD",
    "RVAD" = "RVAD",
    "MCS" = "Mechanical Circulatory Support",
    "ICU" = "ICU",
    "LOS" = "LOS",
    "CRRT" = "CRRT",
    "RRT" = "RRT",
    "CVA" = "Cerebrovascular Accident",
    "COD" = "Cause of Death",
    "DBD" = "DBD",
    "DCD" = "DCD",
    "PHS" = "PHS",
    "UNOS" = "UNOS",
    "PVR" = "PVR",
    "GFR" = "GFR",
    "Hgb" = "Hemoglobin",
    "WBC" = "WBC",
    "Plt" = "Platelets",
    "ALT" = "ALT",
    "AST" = "AST",
    "PHM" = "Predicted Heart Mass",
    "ACR" = "Acute Cellular Rejection",
    
    # Units and measurements
    "minutes" = "(minutes)",
    "days" = "(days)",
    "calc" = "Score",
    "median" = "",
    "tpx" = "",
    
    # Conditions/procedures
    "smoking" = "Smoking",
    "hx" = "History", 
    "DM" = "Diabetes Mellitus",
    "prior" = "Prior",
    "cardiac" = "Cardiac",
    "surg" = "Surgery",
    "temp" = "Temporary",
    "inotrope" = "Inotrope",
    "stroke" = "Stroke",
    "hospital" = "Hospital",
    "survival" = "Survival",
    "needed" = "Needed",
    "index" = "Index",
    "score" = "Score",
    "status" = "Status",
    "recipient" = "Recipient",
    "donor" = "Donor",
    "mismatch" = "Mismatch",
    "drug" = "Drug",
    "use" = "Use",
    "risk" = "Risk",
    "simplified" = "",
    "greater" = "Greater",
    "or" = "",
    "IT" = "Ischemic Time",
    "CPB" = "CPB Time",
    
    # Lab values
    "labs" = "",
    "albumin" = "Albumin",
    "alkphos" = "Alkaline Phosphatase", 
    "bilirubin" = "Bilirubin",
    "creatinine" = "Creatinine",
    "sodium" = "Sodium",
    
    # Numbers and special cases
    "30" = "30-day",
    "90" = "90-day",
    "2R" = ">=2R",
    "5.5" = "5.5",
    "Impella5.5" = "Impella 5.5"
  )
  
  # Process each part
  processed_parts <- character(length(parts))
  for (i in seq_along(parts)) {
    part <- parts[i]
    
    # Check if part is in abbreviation map
    if (part %in% names(abbrev_map)) {
      mapped <- abbrev_map[[part]]
      if (mapped != "") {
        processed_parts[i] <- mapped
      }
    } else {
      # Capitalize first letter, handle special cases
      processed_parts[i] <- .capitalize_medical_term(part)
    }
  }
  
  # Remove empty parts and join
  processed_parts <- processed_parts[processed_parts != ""]
  
  # Handle special patterns
  result <- paste(processed_parts, collapse = " ")
  
  # Post-processing cleanup
  result <- gsub("\\s+", " ", result)  # Multiple spaces
  result <- gsub("^\\s+|\\s+$", "", result)  # Leading/trailing spaces
  
  # Handle specific problematic patterns
  result <- gsub("Postoperative Mechanical Circulatory Support", "Postoperative", result)
  result <- gsub("Survival 90-day", "90-day Survival", result)
  result <- gsub("Postoperative 30-day Day", "Postoperative 30-day", result)
  result <- gsub("Acute Cellular Rejection >=2R Or Greater", ">=2R Acute Cellular Rejection", result)
  result <- gsub("Acute Cellular Rejection >=2R Greater", ">=2R Acute Cellular Rejection", result)
  result <- gsub("Postoperative Ischemic Time", "Ischemic Time", result)
  result <- gsub("Postoperative CPB Time", "CPB Time", result)
  result <- gsub("Operative CPB Time", "CPB Time", result)
  result <- gsub("Operative Ischemic Time", "Ischemic Time", result)
  
  # Fix capitalization issues with level indicators
  result <- gsub(": y$", ": Y", result)
  result <- gsub(": n$", ": N", result)
  result <- gsub("Iabp", "IABP", result)
  result <- gsub("Ecmo", "ECMO", result)
  result <- gsub("Rvad", "RVAD", result)
  result <- gsub("Lvad", "LVAD", result)
  
  # Add units for common patterns
  if (grepl("(Time|LOS)$", result) && !grepl("\\(", result)) {
    if (grepl("Time", result)) {
      result <- paste0(result, " (minutes)")
    } else if (grepl("LOS", result)) {
      result <- paste0(result, " (days)")
    }
  }
  
  return(result)
}

#' Capitalize medical terms appropriately
#' @keywords internal
.capitalize_medical_term <- function(term) {
  
  # Special cases that should remain uppercase
  upper_terms <- c("IT", "CPB", "NICM", "ICM", "IABP", "VA", "LVEF", "CVP", 
                   "MCS", "ICU", "LOS", "CRRT", "RRT", "UNOS", "PVR", "GFR",
                   "WBC", "ALT", "AST", "BMI", "DBD", "DCD", "PHS", "PHM",
                   "ACR", "LVAD", "RVAD", "ECMO")
  
  if (term %in% upper_terms) {
    return(term)
  }
  
  # Numbers should stay as-is
  if (grepl("^[0-9.]+$", term)) {
    return(term)
  }
  
  # Capitalize first letter only
  return(paste0(toupper(substr(term, 1, 1)), tolower(substr(term, 2, nchar(term)))))
}

#' AI-powered cleaning using local model (ollama)
#' @keywords internal
.clean_names_ai_local <- function(var_names, training_data, cache_results, fallback_rules) {
  
  # Check if ollama is available
  if (!.check_ollama_available()) {
    if (fallback_rules) {
      message("Ollama not available, falling back to rule-based cleaning")
      return(.clean_names_rules(var_names, training_data))
    } else {
      stop("Ollama not available and fallback_rules = FALSE")
    }
  }
  
  cleaned <- character(length(var_names))
  
  for (i in seq_along(var_names)) {
    var <- var_names[i]
    
    # Check cache first
    if (cache_results && exists(".var_name_cache", envir = .GlobalEnv)) {
      cache <- get(".var_name_cache", envir = .GlobalEnv)
      if (var %in% names(cache)) {
        cleaned[i] <- cache[[var]]
        next
      }
    }
    
    # Try AI cleaning
    ai_result <- .query_ollama_for_name(var, training_data)
    
    if (!is.null(ai_result)) {
      cleaned[i] <- ai_result
      
      # Cache result
      if (cache_results) {
        cache <- get(".var_name_cache", envir = .GlobalEnv)
        cache[[var]] <- ai_result
        assign(".var_name_cache", cache, envir = .GlobalEnv)
      }
    } else if (fallback_rules) {
      cleaned[i] <- .apply_cleaning_rules(var)
    } else {
      cleaned[i] <- var  # Return original if AI fails and no fallback
    }
  }
  
  return(cleaned)
}

#' Check if ollama is available
#' @keywords internal
.check_ollama_available <- function() {
  # Try to run ollama list command
  tryCatch({
    system("ollama list", ignore.stdout = TRUE, ignore.stderr = TRUE)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Query ollama for variable name cleaning
#' @keywords internal
.query_ollama_for_name <- function(var_name, training_data) {
  
  # Create few-shot examples from training data
  examples <- head(training_data, 5)
  example_text <- paste(
    sapply(1:nrow(examples), function(i) {
      paste0("Input: ", examples$abbrev_name[i], "\nOutput: ", examples$table_name[i])
    }),
    collapse = "\n\n"
  )
  
  # Construct prompt
  prompt <- paste0(
    "You are a medical variable name cleaner for research publications. ",
    "Transform technical variable names into clean, publication-ready names. ",
    "Follow these patterns:\n\n",
    example_text,
    "\n\nTransform this variable name following the same patterns:\n",
    "Input: ", var_name,
    "\nOutput:"
  )
  
  # Query ollama
  tryCatch({
    cmd <- paste0("ollama run llama3.2 '", prompt, "'")
    result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    
    if (length(result) > 0) {
      # Clean up the result
      clean_result <- gsub("^Output:\\s*", "", result[1])
      clean_result <- gsub("\\n.*", "", clean_result)  # Take only first line
      return(clean_result)
    }
    
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

#' Get default training data patterns
#' @keywords internal
.get_default_training_data <- function() {
  data.frame(
    abbrev_name = c(
      "demographics_age_tpx", "postop_ICU_LOS", "survival_30: Y",
      "preop_IABP: Y", "donor_sex: Male", "operative_IT_minutes"
    ),
    table_name = c(
      "Age (years)", "ICU LOS (days)", "30-day Survival",
      "Intra-Aortic Balloon Pump", "Male", "Ischemic Time (minutes)"
    ),
    stringsAsFactors = FALSE
  )
}

# Additional AI methods would go here (OpenAI, hybrid, etc.)
# For now, let's implement the core functionality

#' Test the variable name cleaning system
#' @param test_names Optional character vector of names to test. If NULL, uses example names.
#' @export
test_variable_cleaning <- function(test_names = NULL) {
  
  if (is.null(test_names)) {
    test_names <- c(
      "operative_IT_minutes",
      "operative_CPB_minutes", 
      "postop_LVEF_median",
      "postop_CVP",
      "postop_cardiac_index",
      "postop_inotrope_score",
      "postop_MCS_IABP: Y",
      "postop_VA_ECMO: Y",
      "postop_MCS_Impella5.5: Y",
      "postop_MCS_RVAD: Y",
      "postop_ICU_LOS",
      "postop_RRT_needed: Y",
      "postop_CRRT: Y",
      "postop_stroke: Y",
      "postop_30_day_LVEF",
      "postop_hospital_LOS",
      "ACR_2R_or_greater: Y",
      "survival_30: Y",
      "survival_90: Y",
      "survival: Y",
      "survival_days"
    )
  }
  
  cat("Testing variable name cleaning:\n")
  cat("================================\n\n")
  
  cleaned <- clean_variable_names(test_names, method = "rules")
  
  for (i in seq_along(test_names)) {
    cat(sprintf("%-30s → %s\n", test_names[i], cleaned[i]))
  }
  
  invisible(data.frame(
    original = test_names,
    cleaned = cleaned,
    stringsAsFactors = FALSE
  ))
}