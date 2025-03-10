## Functions used in project

# 1. Closed text analyses of teacher tapp data
# 2. Open text analyses of teacher tapp data

################################################################################
## Functions for open-text coding analyses

# Data prep

# Function to clean responses data
clean_responses_data <- function(data){
  
    clean_data <- data %>%
      
    # Replace % across all columns (except the first one)
    mutate(across(-1, ~ as.numeric(gsub("%", "", .)))) %>%
    
    # Calculate decimal for the last 4 columns
    mutate(across(tail(names(data), 4), ~ . / 100)) %>% 
    
    # Add 'question' column back in
    mutate(question = data$question)
  
  # Return cleaned data
  return (clean_data)
}

# Function to separate data into 8 data frames (one per question option)
sep_options <- function(all_data) {
  
  # Initialize an empty list to store the data frames
  options_dfs <- list()
  
  # Loop through each row in all_data data frame (each row = one option)
  for (i in 1:nrow(all_data)) {
    
    # Get question value string to index options_df list below
    question <- all_data$question[i]
    
    # Create a data frame with column names and the current row of data
    row_data <- all_data[i, -1] # all numeric cols (ignoring question column)
    row_df <- data.frame(row_data) # convert to data frame
    colnames(row_df) <- colnames(all_data[-1]) # assign column names
    
    # Add the created data frame to the list
    options_dfs[[question]] <- row_df
  }
  
  # Return the list of data frames
  return(options_dfs)
  
}

# Function for cleaning margin of error data
clean_moe_data <- function(data){
  
    clean_data <- data %>%
      
    # Replace % across all columns
    mutate(across(everything(), ~ as.numeric(gsub("%", "", .)))) %>%
    
    # Calculate decimals to match proportion data
    mutate(across(everything(), ~ . / 100))
  
  # Return clean data
  return (clean_data)
}

# Function to create long format version of moe data for bar plots
create_longformat_moe <- function(data){
  
  # Use pivot_longer to create long format version of data for plotting
  long_data <- pivot_longer(data,
                           # Include all columns
                           cols = everything(),
                           # Create 'demographic' column from cols
                           names_to = "demographic", 
                           # Put values to 'moe' column
                           values_to = "moe")
  # Return long data
  return(long_data)
}

# Function for creating a list of long format data frames for bar plots
# Arguments:
# - List of data frames containing separated data (one for each question option)
# - Long format moe data
create_longformat_list <- function(separated_options_list,
                                   moe_long_df){
  
  # Create empty list for adding long data frames to
  long_dataframe_list <- list()
  
  # Reshape each data frame in the list to long format
  for (option in names(separated_options_list)) {
    long_df <- pivot_longer(separated_options_list[[option]],
                            # Include all columns
                            cols = everything(),
                            # Create 'demographic' column from cols
                            names_to = "demographic", 
                            # Put values to 'proportion' column
                            values_to = "proportion")
    
    # Join on 'demographic' with moe data
    long_dataframe_list[[option]] <- left_join(long_df, 
                                               moe_long_df,
                                               by = 'demographic')
  }
  
  # Return list of long data frames
  return(long_dataframe_list)
  
}

################################################################################
# Analysis-Specific Functions

# Function: select_data
# Description: Selects data to create a contingency table for each chi-square test.
#
# Arguments:
# - overall_df: Data frame containing overall counts for each demographic.
# - separated_dfs: List of data frames, each corresponding to a question option.
# - question_option: String indicating which question option is of interest.
# - demographic_group: String indicating which demographic group is of interest.

select_data <- function(overall_counts_df, 
                        separated_df_list, 
                        option_str, 
                        demo_str) {
  
  # Select data frame from separated_df_list using option_str
  separated_df <- separated_df_list[[option_str]]
  
  # Select columns of interest in separted_df using demo_str
  demo_df <- select(separated_df,starts_with(demo_str))
  
  # Convert row (corresponding to one question option) to a named vector
  proportions_vector <- as.numeric(demo_df[1, ])
  names(proportions_vector) <- colnames(demo_df)
  
  # Select total counts in overall_counts_df using demo_str
  demo_counts <- select(overall_counts_df,starts_with(demo_str))
  
  # Convert row to named vector
  totalCounts_vector <- as.numeric(demo_counts[1, ])
  names(totalCounts_vector) <- colnames(demo_counts)
  
  # Return list containing these two named vectors
  return(list(proportions_vector = proportions_vector,
              totalCounts_vector = totalCounts_vector))
}

# Function: create_contingency_table
# Description: Creates a contingency table for chi-square testing.
#
# Arguments:
# - vectors_list: A list containing named vectors of proportions and total counts

create_contingency_table <- function(vectors_list){
  
  # Select proportions (that 'selected' the option) and 
  # total counts from provided vectors_list
  proportions <- vectors_list$proportions_vector
  total_counts <- vectors_list$totalCounts_vector
  
  # Convert 'selected' proportions to counts
  selected <-  proportions * total_counts
  
  # Calculated 'notSelected' counts by subtracting 'selected' from total
  notSelected <- total_counts - selected
  
  # Combine into contingency table using `cbind` function
  table <- cbind(selected, notSelected)
  
  # Set col and row names
  colnames(table) <- c("Selected", "Not Selected")
  rownames(table) <- names(vectors_list$proportions_vector)
  
  # Return contingency table, which will be the input to run_chisq_test()
  return(table)
}

# Function: run_chisq_or_fisher_test
# Description: Runs a chi-square test and, if any expected counts are less than 5,
#              runs Fisher's exact test instead.
#              Returns the test result.
#
# Arguments:
# - contingency_table: Contingency table created using create_contingency_table().

run_chisq_test <- function(contingency_table) {
  
  # Perform chi-squared test without returning any results yet
  test <- chisq.test(contingency_table)
  
  # Check if any expected counts are less than 5
  if (any(test$expected < 5)) {
    
    # Print to user if true
    print("Expected counts are <5 in some cells. Using Fisher's exact test.")
    
    # If they are, run fisher's exact (overwrites previous test variable)
    test <- fisher.test(contingency_table, simulate.p.value = TRUE)
  }
  
  # Return the result of the test (chi-square or fisher's)
  return(test)
}

# Function: all_options
# Description: Wrapper function that runs analysis for all options for a 
# given demographic group.
#
# Arguments:
# - demographic_grp_str: String indicating which demographic group to compare.
# - separated_opts_df: List of data frames, each corresponding to a question option.
# - nrespondents_df: Data frame containing overall counts for each demographic.
# - pvalue_adjustment: Specifies the method for p-value adjustment.

all_options <- function(demographic_grp_str, 
                        separated_opts_df, 
                        nrespondents_df,
                        pvalue_adjustment){
  
  # Set up empty significant and all results list.
  sig_options <- list() # to store options that are sig, with their p-value.
  all_results <- list() # to store ALL test results, regardless of sig. level.
  
  # Loop over names in separated options list to loop over each question option.s
  for (option_name in names(separated_opts_df)) {
    
    print (option_name)
    
    # Select data using select_data() function
    proportion_total_vectors <- select_data(nrespondents_df,
                                            separated_opts_df,
                                            option_name,
                                            demographic_grp_str)
    
    # Create contingency table using create_contigency_table() function.
    contingency_table <- create_contingency_table(proportion_total_vectors)
    
    # Run chi-square test using run_chisq_test() function, store in result.
    result <- run_chisq_test(contingency_table)
    
    # Adjust p-value (in place) with p-value adjustment provided.
    result$p.value = result$p.value * pvalue_adjustment
    
    # Add result to all results list.
    all_results[[option_name]] <- result
    
    # If (adjusted) p-value is less than .05, add to significant options list.
    if (result$p.value < 0.05) {
      sig_options[[option_name]] <- result$p.value
    }
  }
  # Return both lists
  return(list(all_results = all_results,
              sig_options = sig_options))
}

# Function: all_demographics
# Description: Wrapper function that runs full analysis for all question options 
# across all demographic groups.
#
# Arguments:
# - list_of_demographics: List of strings indicating which demographic groups to compare.
# - separated_opts_df: List of data frames, each corresponding to a question option.
# - nrespondents_df: Data frame containing overall counts for each demographic.
# - pvalue_adjustment: Specifies the method for p-value adjustment.

all_demographics <- function(list_of_demographics, 
                             separated_opts_df, 
                             nrespondents_df,
                             pvalue_adjustment){
  
  # Empty list for storing ALL results (all demographics, all question options)
  all_results <- list()
  
  # Loop over list of demographic strings to run analysis on each group
  for (demographic_string in list_of_demographics){
    
    print(demographic_string)
    
    # Call all_options() function to run chi-square for each question option
    results <- all_options(demographic_string, 
                           separated_opts_df, 
                           nrespondents_df,
                           pvalue_adjustment)
    
    # Add to all results list
    all_results[[demographic_string]] <- results
  }
  
  # Returns a list of all results: each demographic is the highest level.
  return (all_results)
}

################################################################################
# Plotting

# Line plots
create_facetLine <- function(data,
                             demographic_str, 
                             title_list,
                             demographic_labels_list,
                             question_list,
                             results_list) {
  # Set fontsize
  fontsize <- 7
  
  # Filter data to only include demographic of interest using demographic_str
  data_subset <- data %>%
    filter(str_starts(demographic, demographic_str))
  
  # Set levels of question for ordering
  data_subset$question <- factor(data_subset$question,
                                 levels = c('time' ,
                                            "salary" ,
                                            "internal_training",
                                            "external_training",
                                            "collaborations",
                                            "events",
                                            "recognition",
                                            "none"))
  
  # Order levels of certain demographic groups
  if (demographic_str == 'seniority_'){
    data_subset$demographic <- factor(data_subset$demographic,
                                      levels = c("seniority_teacher",
                                                 "seniority_middle_leader",
                                                 "seniority_slt_excl_head",
                                                 "seniority_headteacher"))
    
    data_subset$question <- factor(data_subset$question,
                                      levels = c("external_training",
                                                 "time",
                                                 "internal_training",
                                                 "collaborations",
                                                 "none",
                                                 "events",
                                                 "salary",
                                                 "recognition"))
  }
  
  if (demographic_str == 'schoolGovernance_'){
    data_subset$demographic <- factor(data_subset$demographic,
                                      levels = c("schoolGovernance_LA_non_comm",
                                                 "schoolGovernance_LA_comm",
                                                 "schoolGovernance_ind_acad",
                                                 "schoolGovernance_small_MAT",
                                                 "schoolGovernance_large_MAT"))
  }
  
  if (demographic_str == 'schoolSize_'){
    data_subset$demographic <- factor(data_subset$demographic,
                                      levels = c("schoolSize_q1_small",
                                                 "schoolSize_q2",
                                                 "schoolSize_q3",
                                                 "schoolSize_q4_large"
                                      ))
  }
  
  if (demographic_str == 'experience_'){
    data_subset$demographic <- factor(data_subset$demographic,
                                      levels = c("experience_Less_than_5",
                                                 "experience_between_5_and_10",
                                                 "experience_between_10_and_20",
                                                 "experience_Over_20"
                                      ))
  }
  
  if (demographic_str == 'ofsted_'){
    data_subset$demographic <- factor(data_subset$demographic,
                                      levels = c("ofsted_RI_inadequate",
                                                 "ofsted_good",
                                                 "ofsted_outstanding"
                                      ))
  }
  
  if (demographic_str == 'seniorSplit_'){
    data_subset$demographic <- factor(data_subset$demographic,
                                      levels = c("seniorSplit_SLT_mentor",
                                                 "seniorSplit_head_teacher_mentor",
                                                 "seniorSplit_SLT_non_Mentor",
                                                 "seniorSplit_head_teacher_non_Mentor"
                                      ))
  }
  
  
  # Add a column to indicate if a question option is significant
  data_subset <- data_subset %>%
    rowwise() %>%
    mutate(is_significant = 
             ifelse(question %in% names(results_list[[demographic_str]]$sig_options),
                    TRUE, FALSE)) %>%
    ungroup() %>%
    mutate(is_significant = factor(is_significant, levels = c(TRUE, FALSE)))
  
  # Create line plot
  plot <- ggplot(data_subset, aes(x = demographic, y = proportion * 100, group = question)) +
    
    # Add lines
    geom_line(aes(color = is_significant), linewidth = 1) +
    
    # Add points
    geom_point(aes(color = is_significant), size = 1) +
    
    # Add error bars (margin of error)
    geom_errorbar(aes(ymin = ifelse(proportion * 100 - moe * 100 < 0, 0, proportion * 100 - moe * 100),
                      ymax = ifelse(proportion * 100 + moe * 100 > 100, 100, proportion * 100 + moe * 100)),
                  position = position_dodge(width = 0.1),
                  width = 0.25, colour = "black", alpha = 0.9, linewidth = 0.5) +
    
    # Set labels and title
    labs(y = "Percentage", x = title_list[[demographic_str]], title = "") +
    scale_x_discrete(labels = function(x) str_replace_all(x, demographic_labels_list)) +
    
    # Change facet labels
    facet_wrap_paginate(~ question, labeller = as_labeller(question_list),
                        nrow = 1, ncol = 8, page = 1) +
    
    # Set to theme_light()
    theme_light() +
    
    # Set theme elements of plot
    theme(
      strip.text = element_text(size = fontsize, colour = 'black'),
      plot.title = element_text(size = fontsize, face = "bold"),
      axis.title.y = element_text(size = fontsize, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(size = fontsize),
      legend.position = 'none',
      axis.title.x = element_text(size = fontsize, face = "bold")
    ) +
    
    scale_y_continuous(breaks = c(0, 50, 100), limits = c(0, 100)) + 
    
    # Color code based on significance
    scale_color_manual(values = c("TRUE" = "#B86BFF", "FALSE" = "black"))
  
  # Return plot and number of facets (rows of data divided by n questions)
  return(list(plots = plot, nfacets = nrow(data_subset) / 8))
}

# Bar plots
create_facetBar <- function(data,
                             demographic_str, 
                             title_list,
                             demographic_labels_list,
                             question_list,
                             results_list) {
  # Set fontsize
  fontsize <- 7
  
  # Filter data to only include demographic of interest using demographic_str
  data_subset <- data %>%
    filter(str_starts(demographic, demographic_str))
  
  data_subset$question <- factor(data_subset$question,
                                 levels = c('time' ,
                                            "salary" ,
                                            "internal_training",
                                            "external_training",
                                            "collaborations",
                                            "events",
                                            "recognition",
                                            "none"))
  
  if (demographic_str == 'seniority_'){
    data_subset$demographic <- factor(data_subset$demographic,
                                      levels = c("seniority_teacher",
                                                 "seniority_middle_leader",
                                                 "seniority_slt_excl_head",
                                                 "seniority_headteacher"))
  }
  
  if (demographic_str == 'schoolGovernance_'){
    data_subset$demographic <- factor(data_subset$demographic,
                                      levels = c("schoolGovernance_LA_non_comm",
                                                 "schoolGovernance_LA_comm",
                                                 "schoolGovernance_ind_acad",
                                                 "schoolGovernance_small_MAT",
                                                 "schoolGovernance_large_MAT"))
  }
  
  if (demographic_str == 'schoolSize_'){
    data_subset$demographic <- factor(data_subset$demographic,
                                      levels = c("schoolSize_q1_small",
                                                 "schoolSize_q2",
                                                 "schoolSize_q3",
                                                 "schoolSize_q4_large"
                                      ))
  }
  
  if (demographic_str == 'experience_'){
    data_subset$demographic <- factor(data_subset$demographic,
                                      levels = c("experience_Less_than_5",
                                                 "experience_between_5_and_10",
                                                 "experience_between_10_and_20",
                                                 "experience_Over_20"
                                      ))
  }
  
  if (demographic_str == 'ofsted_'){
    data_subset$demographic <- factor(data_subset$demographic,
                                      levels = c("ofsted_RI_inadequate",
                                                 "ofsted_good",
                                                 "ofsted_outstanding"
                                      ))
  }
  
  if (demographic_str == 'seniorSplit_'){
    data_subset$demographic <- factor(data_subset$demographic,
                                      levels = c("seniorSplit_SLT_mentor",
                                                 "seniorSplit_head_teacher_mentor",
                                                 "seniorSplit_SLT_non_Mentor",
                                                 "seniorSplit_head_teacher_non_Mentor"
                                      ))
  }
  
  
  # Add a column to indicate if a question option is significant
  data_subset <- data_subset %>%
    rowwise() %>%
    mutate(is_significant = 
             ifelse(question %in% names(results_list[[demographic_str]]$sig_options),
                    TRUE, FALSE)) %>%
    ungroup() %>%
    mutate(is_significant = factor(is_significant, levels = c(TRUE, FALSE)))
  
  # Create bar plot
  plot <- ggplot(data_subset, aes(x = demographic, y = proportion * 100, group = question)) +
    

    geom_bar(aes(color = is_significant, fill = is_significant), position = 'dodge', stat = 'identity') +
    
    # Add error bars (margin of error)
    geom_errorbar(aes(ymin = ifelse(proportion * 100 - moe * 100 < 0, 0, proportion * 100 - moe * 100),
                      ymax = ifelse(proportion * 100 + moe * 100 > 100, 100, proportion * 100 + moe * 100)),
                  position = position_dodge(width = 0.1),
                  width = 0.25, colour = "black", alpha = 0.9, linewidth = 0.5) +
    
    # Set labels and title
    labs(y = "Percentage", title = title_list[[demographic_str]]) +
    scale_x_discrete(labels = function(x) str_replace_all(x, demographic_labels_list)) +
    
    # Change facet labels
    facet_wrap_paginate(~ question, labeller = as_labeller(question_list),
                        nrow = 1, ncol = 8, page = 1) +
    
    # Set to theme_light()
    theme_light() +
    
    # Set theme elements of plot
    theme(
      strip.text = element_text(size = fontsize, colour = 'black'),
      plot.title = element_text(size = fontsize, face = "bold"),
      axis.title.y = element_text(size = fontsize),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(size = fontsize),
      legend.position = 'none',
      axis.title.x = element_blank()
    ) +
    
    scale_y_continuous(breaks = c(0, 50, 100), limits = c(0, 100)) + 
    
    # Color code based on significance
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"))+
    scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "dark grey"))
  
  # Return plot and number of facets (rows of data divided by n questions)
  return(list(plots = plot, nfacets = nrow(data_subset) / 8))
}

################################################################################
# Prettier labels for plots

# Function: process_label
# Description: Processes a string by splitting it at the first underscore, 
#              taking the part after the first underscore, and capitalizing each word.
#
# Arguments:
# - key: A string to be processed.

process_label <- function(key) {
  # Split the string at the first '_'
  parts <- strsplit(key, "_", fixed = TRUE)[[1]]
  # Take the second part (if exists), otherwise the whole key
  label <- if (length(parts) > 1) parts[-1] else parts
  # Combine the parts back into a single string
  label <- paste(label, collapse = " ")
  # Capitalize each word
  label <- tools::toTitleCase(label)
  return(label)
}

# Function: get_processed_labels
# Description: Applies the process_label function to a vector of strings to get 
#             processed labels.
#
# Arguments:
# - keys: A vector of strings to be processed.

get_processed_labels <- function(keys) {
  labels <- sapply(keys, process_label, USE.NAMES = TRUE)
  return(labels)
}

################################################################################

## Functions for open-text coding analyses

# Check how many missing survey responses there are (answer column)
answer_missing <- function(data){
  n_missing <- sum(is.na(data$answer))
  return(n_missing)
}

# Check if rater has missed any responses when coding
coding_missing <- function(data){
  any_blank <- any(apply(data, 1, function(row) all(is.na(row))))
  return(any_blank)
}

# Check for any values that aren't expected characters or integers in codes
check_values <- function(data){
  
  # Create a logical matrix to identify cells that are '1', 1, '0', 0, or NA
  logical_matrix <- data == 1 | data == '1' | is.na(data) | data == '0' | data == 0
  
  # Identify rows that do not contain only the characters/integer set above
  rows_with_unexpected_values <- apply(logical_matrix, 1, function(row) !all(row))
  
  # Return the rows from the original data where the logical condition is TRUE
  unexpected_values <- data[rows_with_unexpected_values, ]
  
  return(unexpected_values)
}

# Fill missing values (NA) with zeros, UNLESS whole row is empty
fill_na <- function(data) {
  data %>%
    mutate(across(everything(), ~ if_else(rowSums(is.na(data)) == ncol(data), .,
                                          replace_na(as.numeric(as.character(.)), 0))))
}

# Identify any codes that have fewer responses than threshold (default 2)
code_redundancy <- function(data, threshold = 2){
  # Identify columns where the sum is less than threshold
  redundant_codes <- colnames(data)[colSums(data) < threshold]
  return(redundant_codes)
}

################################################################################

# Inter rater agreement

# Function to calculate kappa and percent agreement for columns or rows
calculate_agreement <- function(ratings1, ratings2, by = "column") {
  
  # Set N based on 'by' argument (rows or columns)
  n <- if (by == "column") ncol(ratings1) else nrow(ratings1)
  
  # Create empty for results
  kappa_values <- numeric(n)
  percent_agreement <- numeric(n)
  
  for (i in 1:n) {
    if (by == "column") {
      r1 <- ratings1[, i]
      r2 <- ratings2[, i]
    } else {
      r1 <- ratings1[i, ]
      r2 <- ratings2[i, ]
    }
    
    # create combined column or rows
    combined <- if (by == "column") cbind(r1, r2) else t(rbind(r1, r2))
    
    # calculate kappa and % agreement and add to results
    kappa_values[i] <- kappa2(combined, "unweighted")$value
    percent_agreement[i] <- agree(combined)$value
  }
  
  # return results in list
  list(kappa = kappa_values, percent = percent_agreement)
}

# Calculate % of rows and columns with specific kappa agreement levels
agreement_levels <- function(kappa_values, n) {
  agreement <- list(
    perfect = length(which(kappa_values >= 0.81)) / n * 100,
    substantial = length(which(kappa_values >= 0.61 & kappa_values <= 0.80)) / n * 100,
    moderate = length(which(kappa_values >= 0.41 & kappa_values <= 0.60)) / n * 100,
    fair = length(which(kappa_values >= 0.21 & kappa_values <= 0.40))/ n * 100,
    slight = length(which(kappa_values >= 0.01 & kappa_values <= 0.20))/ n * 100,
    none = length(which(kappa_values <= 0)) / n * 100
  )
  
  # Convert agreement levels to a data frame
  agreement_df <- data.frame(
    Category = factor(names(agreement), 
                      levels = c("perfect", "substantial", "moderate", "fair", 
                                 "none", "slight")),
    Percentage = unlist(agreement)
  )
  return(agreement_df)
}

# Function to prepare data and create a pie chart
create_pie_chart <- function(agreement_df, title) {
  
  # Calculate cumulative sum and positions for labels
  agreement_df2 <- agreement_df %>% 
    mutate(csum = rev(cumsum(rev(Percentage))), 
           pos = Percentage / 2 + lead(csum, 1),
           pos = if_else(is.na(pos), Percentage / 2, pos))
  
  # Create a pie chart using ggplot2
  ggplot(agreement_df, aes(x = "" , y = Percentage,
                           fill = fct_inorder(Category))) +
    geom_col(width = 1, color = 1) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Pastel1") +
    geom_label_repel(data = agreement_df2,
                     aes(y = pos, label = paste0(round(Percentage, 1), "%")),
                     size = 4.5, nudge_x = 1, show.legend = FALSE) +
    guides(fill = guide_legend(title = "Agreement Levels")) +
    theme_void() +
    labs(title = title)
}

# Function to prepare data and calculate combined percentages
calculate_combined_percentages <- function(agreement_df) {
  
  # Calculate combined percentages
  combined_percentages <- list(
    substantial_or_higher = sum(agreement_df$Percentage[agreement_df$Category %in% 
                                                          c("perfect",
                                                            "substantial")]),
    moderate_or_higher = sum(agreement_df$Percentage[agreement_df$Category %in%
                                                       c("perfect",
                                                         "substantial",
                                                         "moderate")]),
    fair_or_higher = sum(agreement_df$Percentage[agreement_df$Category %in%
                                                   c("perfect", "substantial",
                                                     "moderate", "fair")])
  )
  
  return(combined_percentages)
}