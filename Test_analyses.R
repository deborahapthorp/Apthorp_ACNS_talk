library(readr)
library(afex)
library(emmeans)
library(knitr)
library(ggplot2)
library(ggrain)

TCH_results_all <- read_csv("docs/data/TCH_electrode_averaged_1to40Hz_results_all.csv")


res_eyes_offset_TCH <- t.test(TCH_results_all$Offset[TCH_results_all$Eye == 0], TCH_results_all$Offset[TCH_results_all$Eye == 1], paired = TRUE)

res_eyes_exponent_TCH <- t.test(TCH_results_all$Exponent[TCH_results_all$Eye == 0], TCH_results_all$Exponent[TCH_results_all$Eye == 1], paired = TRUE)

Exponent_ANOVA <- aov_ez("ID", "Exponent", TCH_results_all, 
                                 within=c("Eye"), between = c("Group"),
                                 factorize = TRUE,
                                 anova_table = list(correction = "none", es = "pes")) 

# Use afex_plot to plot these more simply
exponentPlot <-  afex_plot(Exponent_ANOVA,  ~ Eye,  ~ Group, error = "between")