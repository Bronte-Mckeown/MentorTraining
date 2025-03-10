# Script that creates bar plot of overall percentages for Quant teacher tapp question

################################################################################
library(ggplot2)

# Function for overall bar plot
overall_barplot <- function(data, x_label_list){
  
  fontsize = 7
  
  ggplot(data, aes(x = reorder(Option,-Percentage), y = Percentage)) +
    geom_bar(position = "dodge", stat = "identity", color = "black", size = 0.7, fill = "#007559") +
    geom_errorbar(aes(ymin = ifelse(CI_Lower < 0, 0, CI_Lower),
                      ymax = ifelse(CI_Upper > 100, 100, CI_Upper)), width = 0.25, 
                  position = position_dodge(0.9), color = "black") +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(size = 10,hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = fontsize,face = "bold"),
      axis.title.y = element_text(size = fontsize,face = "bold"),
      axis.text.x = element_text(size = fontsize),
      axis.text.y = element_text(size = fontsize)
    ) +
    labs(
      title = "",
      x = "",
      y = "Percentage"
    )+
    scale_y_continuous(breaks = c(0, 50, 100), limits = c(0, 100))+
    scale_x_discrete(labels = x_label_list)
  
}

################################################################################
data <- read.csv("data/deriv/overall_percentages_teacherTapp_closedquestion_forBarPlot_simp.csv")

################################################################################
# Create X labels that will fit for question options
x_labels <-  c("Additional protected non-contact time (in addition to the statutory requirement for ITE mentors)" = 'Protected Time',
               "Salary contribution" = 'Salary',
               "Opportunities for collaboration with other mentors in the same Trust/region"  = 'Collaboration',
               "Internal training" = 'Internal Training',
               "External training" = 'External Training',
               "Events to celebrate success" = 'Celebration Events',
               "Public recognition of mentoring role, e.g. on school website" = 'Public recognition',
               "None of these"  = 'None of these',
               "Not relevant / cannot answer" = "Not Relevent")

# Call function to create bar plot
overall_bar <- overall_barplot(data, x_labels)
overall_bar

# Save to results folder
ggsave("results/closedResponse_overall_bar_007559.png",
       plot = overall_bar, width = 30, height = 10,
       units = "cm", dpi = 600)
