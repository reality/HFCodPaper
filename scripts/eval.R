library(readr)
library(ggplot2)

library(broom)
library(dplyr)
df <- read_delim("extended_df.tsv", 
                          delim = "\t", escape_double = FALSE, 
                          col_types = cols(text = col_skip()), 
                          trim_ws = TRUE)

df$journal[df$journal == 'PLOS ONE'] <- 'PLoS ONE' 
df$journal[df$journal == 'European Heart Journal. Acute Cardiovascular Care'] <- 'European Heart Journal: Acute Cardiovascular Care'

cor.test(df$publication_year, as.numeric(df$Predict))
cor.test(df$citation_count, as.numeric(df$Predict))

ggplot(df, aes(x = journal, fill = as.factor(Predict))) +
  geom_bar(position = "dodge") +
  xlab("Journal") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual( values = c("#1f77b4", "#ff7f0e"),
                  labels = c("False", "True"),
                  name = "Explicit mention of HF Coding (predicted)")
ggsave('emention.pdf')

ggplot(df, aes(x = publication_year, fill = as.factor(Predict))) +
  geom_bar(position = "dodge") +
  xlab("Year") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual( values = c("#1f77b4", "#ff7f0e"),
                     labels = c("False", "True"),
                     name = "Explicit mention of HF Coding (predicted)")
ggsave('yearmention.pdf')

df$publication_year = as.factor(df$publication_year)
df <- df %>%
  group_by(publication_year, Predict) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(total = sum(count), percentage = (count / total) * 100) %>%
  ungroup()

# Now, adjust the total calculation to be within each publication year
df <- df %>%
  group_by(publication_year) %>%
  mutate(year_total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / year_total)) # Recalculate percentage based on year_total

# Plot with the adjusted percentages
ggplot(df, aes(x = publication_year, y = percentage, fill = as.factor(Predict))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = count, y = percentage), position = position_dodge(width = 0.9), vjust = -0.3) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"),
                    labels = c("False", "True"),
                    name = "Explicit mention of HF Coding (predicted)") +
  xlab("Year") +
  ylab("Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('epercentage.pdf')

# find the most typical

library(scales)
dev.off()
ggplot(df, aes(x = publication_year, y = percentage / 100, fill = as.factor(Predict))) + # Ensure division by 100 if necessary
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = count, y = percentage / 100), position = position_dodge(width = 0.9), vjust = -0.3) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) + # Explicitly set limits and labels for percentages
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"),
                    labels = c("False", "True"),
                    name = "Explicit mention of HF Coding (predicted)") +
  xlab("Year") +
  ylab("Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Calculate the overall proportion of 'true' in the outcome
overall_prop <- mean(df$Predict== T)

# Calculate the proportion of 'true' outcomes for each journal
threshold = 10
journal_props <- df %>%
  group_by(journal) %>%
  summarize(prop_true = mean(Predict == T),
            count = n()) %>%
  # Optionally, filter out journals with very few papers
  filter(count > threshold) %>%
  # Calculate the absolute difference from the overall proportion
  mutate(diff = abs(prop_true - overall_prop)) %>%
  arrange(diff)

# View the journals ordered by how typical they are
print(journal_props)


library(caret)
library(forestplot)

set.seed(1337)

#df$publication_year <- as.factor(df$publication_year)
df$journal <- as.factor(df$journal)
df$journal <- relevel(df$journal, ref = "Heart")
df$Predict <- factor(df$Predict, levels = c(F, T), labels = c("classFalse", "classTrue"))
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
model <- train(Predict ~ publication_year + journal + citation_count, 
               data = df, 
               method = "glm", 
               family = "binomial", 
               trControl = train_control, 
               preProcess = c("center", "scale"))

# Extract Model Coefficients for Forest Plot
coef_df <- as.data.frame(summary(model$finalModel)$coefficients)
names(coef_df) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")

# Prepare data for forest plot
forest_data <- data.frame(
  term = rownames(coef_df),
  estimate = coef_df$Estimate,
  lower = coef_df$Estimate - 1.96 * coef_df$`Std. Error`,
  upper = coef_df$Estimate + 1.96 * coef_df$`Std. Error`,
  pvalue = coef_df$`Pr(>|z|)`
)

# Create the labels for the plot
tabletext <- cbind(
  forest_data$term,
  paste0("Est.= ", round(forest_data$estimate, 2)), 
  paste0("CI [", round(forest_data$lower, 2), ", ", round(forest_data$upper, 2), "]"),
  paste0("p = ", format.pval(forest_data$pvalue, digits = 2))
)

library(forestplot)
# Forest plot
forestplot(labeltext = tabletext, 
           mean = forest_data$estimate, 
           lower = forest_data$lower, 
           upper = forest_data$upper, 
           zero = 0,
           xlab = "Coefficients",
           new_page = TRUE)


# Predict probabilities
library(pROC)
predictions <- predict(model, newdata = df, type = "prob")
#install.packages("pROC")

roc_curve <- roc(df$Predict, predictions$classFalse)
auc(roc_curve)
# Add predicted probabilities to the dataframe
df$predicted_prob = predictions$classTrue

# Bin the predicted probabilities and calculate actual vs predicted in each bin
bin_width = 0.05  # Adjust bin width as needed
calibration_data <- df %>%
  mutate(prob_bin = cut(predicted_prob, breaks = seq(0, 1, by = bin_width), include.lowest = TRUE)) %>%
  group_by(prob_bin) %>%
  summarise(Mean_Predicted = mean(predicted_prob), 
            Observed = mean(as.numeric(Predict) - 1))  # Assuming 'classTrue' is coded as 1

# Calibration plot
ggplot(calibration_data, aes(x = Mean_Predicted, y = Observed)) +
  geom_point() +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  xlim(0, 1) + ylim(0, 1) +
  xlab("Mean Predicted Probability") +
  ylab("Observed Proportion") +
  ggtitle("Calibration Plot") +
  theme_minimal()
ggsave('calibration.pdf')

# Given values
precision <- 0.86
total_predicted_positives <- 909

# Calculate True Positives
true_positives <- precision * total_predicted_positives

# Calculate Standard Error
SE <- sqrt(total_predicted_positives * precision * (1 - precision))

# Z-value for 95% confidence
Z <- 1.96

# Calculate the confidence interval
lower_ci <- true_positives - Z * SE
upper_ci <- true_positives + Z * SE

# Output the results
cat("Estimated True Positives:", true_positives, "\n")
cat("95% Confidence Interval: (", lower_ci, ", ", upper_ci, ")\n", sep="")
