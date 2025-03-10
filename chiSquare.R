# This script runs chi-square tests on the Teacher Tapp quantitative data.
# and creates line chart figures.

################################################################################

# Load libraries
library(tidyverse)
library(ggplot2)
library(patchwork)
library(report)
library(insight)
library(flextable)
library(officer)
library(ragg)
library(ggforce)

# Source utils functions to get functions for cleaning & analysis
source("utils.R")

################################################################################
## Read in data

# Read in responses data, n of responders data, and margin of error data.
responses = read.csv('data/deriv/closedresponse_responses.csv') # 59 cols
n_responders = read.csv('data/deriv/closedresponse_n_responders.csv') # 58 cols
moe = read.csv('data/deriv/closedresponse_moe.csv') # 58 cols

# Check col names match between csv files
all(colnames(responses[2:59]) == colnames(moe))
all(colnames(moe) == colnames(n_responders))

################################################################################
## Set variables for analysis

# Set vector of demographic strings of interest (phase and seniority)
# Chi-square/Fisher's tests will be run on these demographic groups
demographic_list <- c('seniority_', 'phase_')

# Set p-value adjustment for this analysis
# Most stringent would be N options * length of demographic_list above
pvalue_adjust_value = 4
pvalue_adjust_value

################################################################################
## Data Prep

# Clean up Responses data (last four cols specifically)
responses_cleaned_data <- clean_responses_data(responses)

# Create list of separate data frames for each question option from cleaned data
separated_options <- sep_options(responses_cleaned_data)
separated_options <- separated_options[c("internal_training", "external_training")]

# Clean up margin of error data for bar plots
moe_cleaned_data <- clean_moe_data(moe)

# Create long format version of moe data for bar plots
moe_long <- create_longformat_moe(moe_cleaned_data)

# Create list of long format data frames of separated option data and join
# with moe_long for bar plots
# long_dataframes <- create_longformat_list(separated_options, moe_long)

# Create long version of responses data and join with moe_long
responses_long <- pivot_longer(responses_cleaned_data,
                               # # Keep 'question' as an identifier column
                               cols = -question,
                               # Create 'demographic' column from cols
                               names_to = "demographic", 
                               # Put values to 'proportion' column
                               values_to = "proportion")

# Join on demographic with moe data
responses_long <- left_join(responses_long, moe_long,by = c('demographic'))

# Subset to training
responses_long <- responses_long %>%
  filter(question == 'internal_training'| question == 'external_training')

################################################################################
## Sanity check of Analysis before running all options 

## Sanity check using an example test
internal_training_phase_data <- select_data(n_responders, 
                    separated_options, 
                    "internal_training", 
                    "phase_") 

internal_training_phase_contingency <- create_contingency_table(internal_training_phase_data)

result <- run_chisq_test(internal_training_phase_contingency)
result

################################################################################

# Call all_demographics function to get a list of ALL results
# All demographics and all question options
all_results_list <- all_demographics(demographic_list, 
                                     separated_options,
                                     n_responders,
                                     pvalue_adjust_value)

################################################################################
# Making pretty labels

# Create named vector of pretty demographics for plotting and tables
pretty_demographics = c('seniority_' =  'Teacher Seniority',
                        'phase_'= 'School Phase'
                        )

# Create named vector of nicer titles for plotting purposes and tables
pretty_questions = c(
                     "internal_training" = 'Internal Training',
                     "external_training" = 'External Training'
                     )

# Get pretty labels
keys_to_process <- colnames(responses_cleaned_data)
pretty_demographic_labels <- get_processed_labels(keys_to_process)

################################################################################
# Create empty list to store lines
line_list <- list()

# Loop over demographic list (pretty_demographics)
for (demo_name in names(pretty_demographics)) {
  
  # Create facet bar graph for that demographic variable
  facet_line <- create_facetLine(responses_long,
                               demo_name,
                               pretty_demographics,
                               pretty_demographic_labels,
                               pretty_questions,
                               all_results_list)
  
  # Save to results folder
  ggsave(
    filename = sprintf('results/teacherTapp_quant/linecharts/facet_%s.png',
    demo_name),
    plot = facet_line$plots,
    width = 7, # Fixed width to ensure consistent facet sizes
    height = 7,
    units = 'cm',
    dpi = 1000,
    device = ragg::agg_png,
    scaling = 1
  )
  
  # Add facet bar to bar_list
  line_list[[demo_name]] <- facet_line$plots
}

################################################################################
# Put plots together

TrainingFigure <- line_list$phase_ / line_list$seniority_ + 
  plot_annotation(title = 'What does your school do to support mentors to undertake their ITE mentoring role?',
                  theme = theme(plot.title = element_text(size = 10, hjust = 0.5,
                                                          face = "bold")),
                  tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 10))
  
TrainingFigure

ggsave(
  filename = sprintf('results/teacherTapp_quant/training_phase_seniority_lines.png'),
  plot = TrainingFigure,
  width = 15,
  height = 15,
  units = 'cm',
  dpi = 1000,
  device = ragg::agg_png,
  scaling = 1
)
################################################################################

# Create empty list to store bars
bar_list <- list()

# Loop over demographic list (pretty_demographics)
for (demo_name in names(pretty_demographics)) {
  
  # Create facet bar graph for that demographic variable
  facet_bar <- create_facetBar(responses_long,
                                 demo_name,
                                 pretty_demographics,
                                 pretty_demographic_labels,
                                 pretty_questions,
                                 all_results_list)
  
  # Save to results folder
  # Flexibly determines size of figure by number of facets
  ggsave(
    filename = sprintf('results/teacherTapp_quant/barcharts/facet_%s.png', demo_name),
    plot = facet_bar$plots,
    width = 20, # Fixed width to ensure consistent facet sizes
    height = 7,
    units = 'cm',
    dpi = 1000,
    device = ragg::agg_png,
    scaling = 1
  )
  
  # Add facet bar to bar_list
  bar_list[[demo_name]] <- facet_bar$plots
}

# Put plots together

TrainingFigure <- bar_list$phase_ / bar_list$seniority_ + 
  plot_annotation(title = 'What does your school do to support mentors to undertake their ITE mentoring role?',
                  theme = theme(plot.title = element_text(size = 10, hjust = 0.5,
                                                          face = "bold")),
                  tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 10))

TrainingFigure

ggsave(
  filename = sprintf('results/teacherTapp_quant/training_phase_seniority_bars.png'),
  plot = TrainingFigure,
  width = 15,
  height = 15,
  units = 'cm',
  dpi = 1000,
  device = ragg::agg_png,
  scaling = 1
)
################################################################################
# Reporting

# Get version numbers of packages used
report(sessionInfo())

# Initialize empty data frames for chi_results_table and fishers_results_table
chi_results_table <- data.frame()
fishers_results_table <- data.frame()

# Loop through each demographic in all_results_list
for (demographic_name in names(all_results_list)) {
  demographic <- all_results_list[[demographic_name]]
  
  # Loop through each result in the demographic's all_results
  for (result_name in names(demographic$all_results)) {
    
    # Select result
    result <- demographic$all_results[[result_name]]
    
    # Create a list with demographics and question options to add to
    report_result <- list(
      Demographic = pretty_demographics[demographic_name],
      Option = pretty_questions[result_name]
    )
    
    # Add results to different tables depending on whether they are chi or fishers
    if (result$method %in% c("Pearson's Chi-squared test",
         "Pearson's Chi-squared test with Yates' continuity correction")) {
      
      # Get the report_table result (using report library)
      report_result <- c(report_result, report_table(result))
      
      # Append the report_table result to chi_results_table
      chi_results_table <- rbind(chi_results_table, report_result)
      
    } else if (result$method == 
      "Fisher's Exact Test for Count Data with simulated p-value\n\t (based on 2000 replicates)") {
      
      # Add p-value to the report result (only values available)
      report_result$p <- result$p.value
      
      # Append the report result to fishers_results_table
      fishers_results_table <- rbind(fishers_results_table, report_result)
    }
    
  }
}

# Use format_table to "prettify" data frame
chi_results_table <-insight::format_table(chi_results_table, stars = TRUE)

# Order chi table so that within each demographic, questions are ordered according 
# to effect size
chi_results_table <- chi_results_table %>%
  mutate(`Cramer's V (adj.)` = as.numeric(`Cramer's V (adj.)`),
         `Chi2` = as.numeric(`Chi2`)) %>%
  group_by(Option) %>%
  arrange(Demographic, desc(`Cramer's V (adj.)`))%>%
  ungroup()

chi_results_table <- chi_results_table %>%
  mutate(`Cramer's V (adj.)` = sprintf("%.2f", `Cramer's V (adj.)`),
         `Chi2` = sprintf("%.2f", `Chi2`))

# Neaten chi-square table (changing values and col names)
chi_results_table$Method[chi_results_table$Method =="Pearson's Chi-squared test"] <- "No"
chi_results_table$Method[chi_results_table$Method ==
    "Pearson's Chi-squared test with Yates' continuity correction"] <-
    "Yes"
colnames(chi_results_table)[6] <- "Correction" # Changes Method to Correction
colnames(chi_results_table)[8] <- "Cramer's V (adj.) CI"

# Create a flextable for chi table
chi_ft <- flextable(chi_results_table)
chi_ft

# Identify the rows where the demographic group changes
demographic_changes <- which(chi_results_table$Demographic != 
                              lag(chi_results_table$Demographic,
                              default = first(chi_results_table$Demographic)))

# Add horizontal lines where the demographic group changes
for (i in demographic_changes) {
  chi_ft <- chi_ft %>%
    hline(i = i-1, border = fp_border(width = 1))
}

# Merge Demographic column to stop repeating values
merged_ft <- merge_v(chi_ft, j = c("Demographic"))
merged_ft

# Save the flextable to a Word document
chi_doc <- read_docx() %>%
  body_add_flextable(merged_ft) %>%
  body_add_par("Chi-Squared Results Table", style = "Normal")

print(chi_doc, target = "results/teacherTapp_quant/chi_results_table.docx")