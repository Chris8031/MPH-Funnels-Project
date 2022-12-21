###################################################
# Preamble
library(tidyverse)
library(xtable)
# Predefined Variables
N = c(50, 100, 200)
denom_range = list(c(1,50), c(51,100), c(101, 200), c(201,500), c(501, 1000), 
                   c(1001, 5000), c(5001,10000))
denom_range_levels = map_chr(unique(denom_range),paste,collapse="-")
#####################################################################
# Load Dataset
confusion_matrix_iter <- get(load("D:/Github/MPH-Funnels-Project/Andrew's Work/ConfusionMatrixByIter.RData"))
######################################################################
remove_underscore.fn <- function(x) {
  gsub("_", " ", x)
}

confusion_matrix <- confusion_matrix_iter %>%
  mutate(PPV = true_positives / (true_positives + false_positives + .Machine$double.eps),
         NPV = true_negatives / (true_negatives + false_negatives),
         MCC_numerator = (true_positives * true_negatives - false_positives * false_negatives),
         MCC_denominator = sqrt((true_positives + false_positives) * (true_positives + false_negatives)
                * (true_negatives + false_positives) * (true_negatives + false_negatives)),
         MCC = MCC_numerator / (MCC_denominator + .Machine$double.eps)
         ) %>%
  select(-c(MCC_numerator, MCC_denominator)) %>%
  group_by(N, denom_range, outlier_sd_diff, DataType, Adjustment) %>%
  summarise(across(true_positives:MCC, mean, na.rm =TRUE)) %>%
  mutate(denom_range = factor(denom_range, levels=denom_range_levels),
         Adjustment = factor(Adjustment, levels=c("unadj","add","mult","laney","vidmar"),
                             labels=c("Unadjusted","Additive","Multiplicative",
                                      "Laney's","Vidmar's")),
         outlier_sd_diff = factor(outlier_sd_diff, levels=seq(from=2, to=4, by=0.5),
                                  labels=paste0(seq(from=2, to=4, by=0.5), "SD")),
         N = factor(N)) %>%
  rename_with(remove_underscore.fn) %>%
  view()

glimpse(confusion_matrix)

tabular_results.fn <- function(value, sd) {
  remove_underscore.fn <- function(x) {
    gsub("_", " ", x)
  }
  confusion_matrix_iter <- get(load("D:/Github/MPH-Funnels-Project/Andrew's Work/ConfusionMatrixByIter.RData"))
  ####
  confusion_matrix <- confusion_matrix_iter %>%
    mutate(PPV = true_positives / (true_positives + false_positives + .Machine$double.eps),
           NPV = true_negatives / (true_negatives + false_negatives),
           MCC_numerator = (true_positives * true_negatives - false_positives * false_negatives),
           MCC_denominator = sqrt((true_positives + false_positives) * (true_positives + false_negatives)
                                  * (true_negatives + false_positives) * (true_negatives + false_negatives)),
           MCC = MCC_numerator / (MCC_denominator + .Machine$double.eps)
    ) %>%
    select(-c(MCC_numerator, MCC_denominator)) %>%
    group_by(N, denom_range, outlier_sd_diff, DataType, Adjustment) %>%
    summarise(across(true_positives:MCC, mean, na.rm =TRUE)) %>%
    mutate(denom_range = factor(denom_range, levels=denom_range_levels),
           Adjustment = factor(Adjustment, levels=c("unadj","add","mult","laney","vidmar"),
                               labels=c("Unadjusted","Additive","Multiplicative",
                                        "Laney's","Vidmar's")),
           outlier_sd_diff = factor(outlier_sd_diff, levels=seq(from=2, to=4, by=0.5),
                                    labels=paste0(seq(from=2, to=4, by=0.5), "SD")),
           N = factor(N)) %>%
    rename_with(remove_underscore.fn)  %>%
    group_by(N, `denom range`, Adjustment) %>%
    filter(DataType == value, 
           `outlier sd diff` == paste0(sd, "SD"),
           `denom range` == "1-50" ) %>%
    summarise(across(`true positives`:MCC, mean, na.rm =TRUE)) %>%
    rename(`Denominator Range` = `denom range`,
           `True Positives` = `true positives`,
           `False Negatives` = 'false negatives',
           `False Positives` = `false positives`,
           `True Negatives` = 'true negatives',
           `Sensitivity` = sensitivity,
           `Specificity` = specificity) %>%
  # Decided to drop TP:FN
    select(-c(`True Positives`, `False Negatives`, `False Positives`, `True Negatives`))
  xtable1 <- xtable::xtable(confusion_matrix,
                            caption = paste("Simulation Results for", if_else(value == "sr", 
                                                                              "Standardised-Ratio",
                                                                              "Proportion"), 
                "Indicators where the Outlier Standard Difference is", sd, 
               "SD and denominator range is between 1-50."),
                            label = paste(value, sd),
                            digits = 3,
                            align = "|l|l|X|X|l|l|l|l|l|")
  filename <- paste("D:/Github/MPH-Funnels-Project/Simulations/Final Simulations/Simulation Results/", value, sd, ".txt")
  print.xtable(xtable1,
               caption.placement = getOption("xtable.caption.placement", "top"),
               include.rownames = getOption("xtable.include.rownames", FALSE),
               file = getOption("xtable.file", filename),
               floating.environment = getOption("xtable.floating.environment", "table*"),
               table.placement = getOption("xtable.table.placement", "htbp"),
               tabular.environment = getOption("xtable.tabular.environment", "tabularx"),
               width = getOption("xtable.width", "\\linewidth"),
               hline.after = getOption("xtable.hline.after", c(-1,0, 
                                                               seq(ifelse(value == "pr", 5, 4), nrow(xtable1), 
                                                                   ifelse(value == "pr", 5, 4))
                                                               )))
}


###########################################################
# print tables 
tabular_results.fn("pr", 2)
tabular_results.fn("pr", 2.5)
tabular_results.fn("pr", 3)
tabular_results.fn("pr", 3.5)
tabular_results.fn("pr", 4)
tabular_results.fn("sr", 2)
tabular_results.fn("sr", 2.5)
tabular_results.fn("sr", 3)
tabular_results.fn("sr", 3.5)
tabular_results.fn("sr", 4)
#####################################################################
make_summary_tables <- function(value) {
  remove_underscore.fn <- function(x) {
    gsub("_", " ", x)
  }
  confusion_matrix_iter <- get(load("D:/Github/MPH-Funnels-Project/Andrew's Work/ConfusionMatrixByIter.RData"))
  ####
  confusion_matrix <- confusion_matrix_iter %>%
    mutate(PPV = true_positives / (true_positives + false_positives + .Machine$double.eps),
           NPV = true_negatives / (true_negatives + false_negatives),
           MCC_numerator = (true_positives * true_negatives - false_positives * false_negatives),
           MCC_denominator = sqrt((true_positives + false_positives) * (true_positives + false_negatives)
                                  * (true_negatives + false_positives) * (true_negatives + false_negatives)),
           MCC = MCC_numerator / (MCC_denominator + .Machine$double.eps)
    ) %>%
    select(-c(MCC_numerator, MCC_denominator)) %>%
    group_by(N, denom_range, outlier_sd_diff, DataType, Adjustment) %>%
    summarise(across(true_positives:MCC, mean, na.rm =TRUE)) %>%
    mutate(denom_range = factor(denom_range, levels=denom_range_levels),
           Adjustment = factor(Adjustment, levels=c("unadj","add","mult","laney","vidmar"),
                               labels=c("Unadjusted","Additive","Multiplicative",
                                        "Laney's","Vidmar's")),
           outlier_sd_diff = factor(outlier_sd_diff, levels=seq(from=2, to=4, by=0.5),
                                    labels=paste0(seq(from=2, to=4, by=0.5), "SD")),
           N = factor(N)) %>%
    rename_with(remove_underscore.fn)  %>%
    group_by(DataType, `outlier sd diff`, N, `denom range`, Adjustment) %>%
    filter(DataType == value, 
           `denom range` == "1-50" ) %>%
    summarise(across(`true positives`:MCC, mean, na.rm =TRUE)) %>%
    rename(`Denominator Range` = `denom range`,
           `True Positives` = `true positives`,
           `False Negatives` = 'false negatives',
           `False Positives` = `false positives`,
           `True Negatives` = 'true negatives',
           `Sensitivity` = sensitivity,
           `Specificity` = specificity) %>%
    # Decided to drop TP:FN
    select(-c(`True Positives`, `False Negatives`, `False Positives`, `True Negatives`)) %>%
    group_by(N, `outlier sd diff`) %>%
    filter(MCC == max(MCC)) %>%
    select(-DataType) %>%
    rename(`Outlier Standard Deviation` = `outlier sd diff`)
}

view(make_summary_tables("sr"))
make_summary_tables("pr")


create_LaTeX_summary <- function(value) {
  data <- make_summary_tables(value) %>%
    select(-c(PPV, NPV))
  xtable1 <- xtable::xtable(data,
caption = paste(
  "The best performing model according to the Matthews Correlation Coefficient for",
                if_else(value == "sr", 
                        "Standardised-Ratio",
                        "Proportion"), 
                "indicators, grouped by outlier standard deviation difference and sample size (N), where the denominator range is between 1-50."),
                            label = paste(value, "_overall"),
                            digits = 3,
                            align = "|l|X|l|X|X|l|l|l|")
  filename <- paste("D:/Github/MPH-Funnels-Project/Simulations/Final Simulations/Simulation Results/", value, "overall.txt")
  print.xtable(xtable1,
               caption.placement = getOption("xtable.caption.placement", "top"),
               include.rownames = getOption("xtable.include.rownames", FALSE),
               file = getOption("xtable.file", filename),
               floating.environment = getOption("xtable.floating.environment", "table*"),
               table.placement = getOption("xtable.table.placement", "htbp"),
               tabular.environment = getOption("xtable.tabular.environment", "tabularx"),
               width = getOption("xtable.width", "\\linewidth"),
               hline.after = getOption("xtable.hline.after", c(-1,0, 
seq(3, nrow(xtable1), 3)
               )))
}
create_LaTeX_summary("sr")
create_LaTeX_summary("pr")
    
########################################################################
Figures

remove_underscore.fn <- function(x) {
    gsub("_", " ", x)
  }
confusion_matrix_iter <- get(load("D:/Github/MPH-Funnels-Project/Andrew's Work/ConfusionMatrixByIter.RData"))
  ####
  confusion_matrix_SR <- confusion_matrix_iter %>%
    mutate(PPV = true_positives / (true_positives + false_positives + .Machine$double.eps),
           NPV = true_negatives / (true_negatives + false_negatives),
           MCC_numerator = (true_positives * true_negatives - false_positives * false_negatives),
           MCC_denominator = sqrt((true_positives + false_positives) * (true_positives + false_negatives)
                                  * (true_negatives + false_positives) * (true_negatives + false_negatives)),
           MCC = MCC_numerator / (MCC_denominator + .Machine$double.eps)
    ) %>%
    dplyr::select(-c(MCC_numerator, MCC_denominator)) %>%
    group_by(N, denom_range, outlier_sd_diff, DataType, Adjustment) %>%
    summarise(across(true_positives:MCC, mean, na.rm =TRUE)) %>%
    mutate(denom_range = factor(denom_range, levels=denom_range_levels),
           Adjustment = factor(Adjustment, levels=c("unadj","add","mult","laney","vidmar"),
                               labels=c("Unadjusted","Additive","Multiplicative",
                                        "Laney's","Vidmar's")),
           outlier_sd_diff = factor(outlier_sd_diff, levels=seq(from=2, to=4, by=0.5),
                                    labels=paste0(seq(from=2, to=4, by=0.5), "SD")),
           N = factor(N)) %>%
    rename_with(remove_underscore.fn)  %>%
    group_by(DataType, `outlier sd diff`, N, `denom range`, Adjustment) %>%
    filter(DataType == "sr", 
           `denom range` == "1-50" ) %>%
    summarise(across(`true positives`:MCC, mean, na.rm =TRUE)) %>%
    rename(`Denominator Range` = `denom range`,
           `True Positives` = `true positives`,
           `False Negatives` = 'false negatives',
           `False Positives` = `false positives`,
           `True Negatives` = 'true negatives',
           `Sensitivity` = sensitivity,
           `Specificity` = specificity) %>%
    # Decided to drop TP:FN
    dplyr::select(-c(`True Positives`, `False Negatives`, `False Positives`, `True Negatives`)) 

confusion_matrix_SR %>%
    ggplot(aes(
      x = `outlier sd diff`, y = MCC, color = Adjustment, group = Adjustment
    )) +
    geom_line() +
  geom_point() +
    theme_bw() +
  facet_wrap(vars(N), ncol = 1, scales = "free") +
  labs(y = "Matthews Correlation Coefficient",
       x = "Outlier Standard Deviation Difference",
       color = "Adjustment Model")

ggsave("mcc_sd.png",
       path = "D:/Github/MPH-Funnels-Project/Simulations/Final Simulations",
       width = 20,
       height = 15,
       units = "cm")



confusion_matrix <- confusion_matrix_iter %>%
  mutate(PPV = true_positives / (true_positives + false_positives + .Machine$double.eps),
         NPV = true_negatives / (true_negatives + false_negatives),
         MCC_numerator = (true_positives * true_negatives - false_positives * false_negatives),
         MCC_denominator = sqrt((true_positives + false_positives) * (true_positives + false_negatives)
                                * (true_negatives + false_positives) * (true_negatives + false_negatives)),
         MCC = MCC_numerator / (MCC_denominator + .Machine$double.eps)
  ) %>%
  dplyr::select(-c(MCC_numerator, MCC_denominator)) %>%
  group_by(N, denom_range, outlier_sd_diff, DataType, Adjustment) %>%
  summarise(across(true_positives:MCC, mean, na.rm =TRUE)) %>%
  mutate(denom_range = factor(denom_range, levels=denom_range_levels),
         Adjustment = factor(Adjustment, levels=c("unadj","add","mult","laney","vidmar"),
                             labels=c("Unadjusted","Additive","Multiplicative",
                                      "Laney's","Vidmar's")),
         outlier_sd_diff = factor(outlier_sd_diff, levels=seq(from=2, to=4, by=0.5),
                                  labels=paste0(seq(from=2, to=4, by=0.5), "SD")),
         N = factor(N)) %>%
  rename_with(remove_underscore.fn)  %>%
  group_by(DataType, `outlier sd diff`, N, `denom range`, Adjustment) %>%
  filter(DataType == "pr", 
         `denom range` == "1-50" ) %>%
  summarise(across(`true positives`:MCC, mean, na.rm =TRUE)) %>%
  rename(`Denominator Range` = `denom range`,
         `True Positives` = `true positives`,
         `False Negatives` = 'false negatives',
         `False Positives` = `false positives`,
         `True Negatives` = 'true negatives',
         `Sensitivity` = sensitivity,
         `Specificity` = specificity) %>%
  # Decided to drop TP:FN
  dplyr::select(-c(`True Positives`, `False Negatives`, `False Positives`, `True Negatives`)) %>%
  view()

confusion_matrix %>%
  ggplot(aes(
    x = `outlier sd diff`, y = MCC, color = Adjustment, group = Adjustment
  )) +
  geom_line() +
  geom_point() +
  theme_bw() +
  facet_wrap(vars(N), ncol = 1, scales = "free") +
  labs(y = "Matthews Correlation Coefficient",
       x = "Outlier Standard Deviation Difference",
       color = "Adjustment Model")

ggsave("mcc_pr.png",
       path = "D:/Github/MPH-Funnels-Project/Simulations/Final Simulations",
       width = 20,
       height = 15,
       units = "cm")
