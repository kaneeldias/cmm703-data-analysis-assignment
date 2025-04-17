#' ---
#' title: "CMM703 - Data Analysis Coursework "
#' author: "Kaneel Dias"
#' date: "2025-04-17"
#' ---
suppressWarnings(suppressMessages({
  library(ggplot2)
  require(gridExtra)
  library(glue)
  library(ggcorrplot)
  library(vcd)
  library(tidyr)
  library(dplyr)
  library(pheatmap)
  library(caTools)
  library(pROC)
  library(shiny)
}))

#' # TASK 1: CANDY DATASET
#' The objective of this exercise would be to determine the effect that the variables contained within the dataset 
#' have on the target value `winpercent`. All visualizations included in this report will be made with that objective 
#' in mind.

candy_data <- read.csv("~/CMM703/candy-data.csv")


#' ## 1.1 Effect of the categorical variables
#' First we should convert all the categorical columns, from numeric to factor.
candy_data$chocolate <- as.factor(candy_data$chocolate)
candy_data$fruity <- as.factor(candy_data$fruity)
candy_data$caramel <- as.factor(candy_data$caramel)
candy_data$peanutyalmondy <- as.factor(candy_data$peanutyalmondy)
candy_data$nougat <- as.factor(candy_data$nougat)
candy_data$crispedricewafer <- as.factor(candy_data$crispedricewafer)
candy_data$hard <- as.factor(candy_data$hard)
candy_data$bar <- as.factor(candy_data$bar)
candy_data$pluribus <- as.factor(candy_data$pluribus)

#' Next, we can plot boxplots for each categorical variable, and the effect it has on the winning percentage.
plot_categorical_boxplot <- function(data, variable) {
  plot <- ggplot(data=data, aes(x=data[,variable], y=winpercent)) + 
    xlab(variable) +
    geom_boxplot()
  
  return(plot)
}

plot_all_categorical_boxplots <- function(data) {
  chocolate_plot <- plot_categorical_boxplot(data, "chocolate")
  fruity_plot <- plot_categorical_boxplot(data, "fruity")
  caramel_plot <- plot_categorical_boxplot(data, "caramel")
  peanutyalmondy_plot <- plot_categorical_boxplot(data, "peanutyalmondy")
  nougat_plot <- plot_categorical_boxplot(data, "nougat")
  crispedricewafer_plot <- plot_categorical_boxplot(data, "crispedricewafer")
  hard_plot <- plot_categorical_boxplot(data, "hard")
  pluribus_plot <- plot_categorical_boxplot(data, "pluribus")
  
  
  grid.arrange(chocolate_plot, fruity_plot, caramel_plot, peanutyalmondy_plot, nougat_plot, crispedricewafer_plot, hard_plot, pluribus_plot, nrow = 3)
}

#+ fig.height = 20, fig.width = 15
plot_all_categorical_boxplots(candy_data)

#' ### 1.1.1 Potential improvements
#' We can suggest the following improvements  
#' - Visually separate the `contains (1)` and `does not contain (0)` plots using colours  
#' - Indicate for each boxplot, the  
#'       - Count of records with that value  
#'       - The mean of the `winpercent` value  
#' - Indicate the effect that variable has on the `winpercent` using ANOVA  
#'       - Include the F value  
#'       - Include the P value  


get_summary_stats <- function(y) {
  bxp_stats <- boxplot.stats(y)
  upper_whisker <- bxp_stats$stats[5]
  max_val <- max(c(upper_whisker, bxp_stats$out), na.rm = TRUE)
  
  n <- length(y)
  q1 <- quantile(y, 0.25, na.rm = TRUE, names = FALSE)
  avg <- mean(y, na.rm = TRUE)
  q3 <- quantile(y, 0.75, na.rm = TRUE, names = FALSE)
  
  label_str <- paste(
    paste("n =", n),
    paste("u =", round(avg, 2)),
    sep = "\n"
  )
  
  return(data.frame(
    y = max_val,
    label = label_str
  ))
}

plot_categorical_boxplot_improved <- function(data, variable) {
  anova <- aov(reformulate(variable, "winpercent"), data=data)
  p_val <- summary(anova)[[1]][["Pr(>F)"]][1]
  f_val <- summary(anova)[[1]][["F value"]][1]
  anova_text <- glue("ANOVA results\nF = {round(f_val, 2)}\np = {format(round(p_val, 5), nsmall = 5)}")
  
  plot <- ggplot(data=data, aes(x=data[,variable], y=winpercent, col=data[,variable])) + 
    labs(
      title = glue('Effect of {variable} on win percentage'),
      x = variable
    ) +
    geom_boxplot() +
    scale_color_manual(values = c("#e74c3c", "#2ecc71")) +
    theme(
      legend.position="none",
      plot.title = element_text(color = "#0099f8", size = 12, face = "bold", hjust = 0.5),
    )
  
  plot <- plot + stat_summary(
    fun.data = get_summary_stats,
    geom = "text",
    hjust = 0.5,
    vjust = -0.5,
    size = 3,       
    color = "black"
  ) 
  
  plot <- plot + annotate(
    geom = "text",
    x = -Inf,
    y = Inf,      
    label = anova_text,
    hjust = -0.1,       
    vjust = 1.5,      
    size = 3,       
    color = "black"
  ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.4))) # More space at top
  
  
  return(plot)
}

plot_all_categorical_boxplots_improved <- function(data) {
  chocolate_plot <- plot_categorical_boxplot_improved(data, "chocolate")
  fruity_plot <- plot_categorical_boxplot_improved(data, "fruity")
  caramel_plot <- plot_categorical_boxplot_improved(data, "caramel")
  peanutyalmondy_plot <- plot_categorical_boxplot_improved(data, "peanutyalmondy")
  nougat_plot <- plot_categorical_boxplot_improved(data, "nougat")
  crispedricewafer_plot <- plot_categorical_boxplot_improved(data, "crispedricewafer")
  hard_plot <- plot_categorical_boxplot_improved(data, "hard")
  pluribus_plot <- plot_categorical_boxplot_improved(data, "pluribus")
  
  
  grid.arrange(chocolate_plot, fruity_plot, caramel_plot, peanutyalmondy_plot, nougat_plot, crispedricewafer_plot, hard_plot, pluribus_plot, nrow = 3)
}

#+ fig.height = 20, fig.width = 15
plot_all_categorical_boxplots_improved(candy_data)

#' ### 1.1.2 Insights
#' Here we can see that the `chocolate` variable has the highest effect on `winpercent` (highest f-value), and it has a very 
#' low p-value as well, indicating that it is most likely to be having an effect. On the other hand, `nougat` has a p-value > 0.05,
#' (as well as a low f-value) which indicates that its effect on the winning percentage is not likely.
#'

#' ## 1.2 Effect of the numeric variables
#' There are two numeric, continuous variables: `sugarpercent` and `pricepercent` We can visualize their effect on `winpercent` using scatter plots.
plot_numeric_scatterplot <- function(data, variable) {
  plot <- ggplot(data=candy_data, aes(x=data[,variable], y=winpercent)) +
    labs(
      x = variable
    ) +
    geom_point()
  
  return (plot)
}

plot_all_numerical_scatterplots <- function(data) {
  sugar_plot <- plot_numeric_scatterplot(data, "sugarpercent")
  price_plot <- plot_numeric_scatterplot(data, "pricepercent")
  
  
  
  grid.arrange(sugar_plot, price_plot, nrow = 1)
}

#+ fig.height = 10, fig.width = 20
plot_all_numerical_scatterplots(candy_data)


get_r2 <- function (x, y) cor(x, y) ^ 2

#' ### 1.2.1 Potential Improvements
#' We can suggest the following improvements:  
#' - Add a regression line to be able to view the relationship between the two variables and the winning percentage  
#' - Include the R² value (coeffiecient of determination in the chart) to determine whether the they correlate  

plot_numeric_scatterplot_improved <- function(data, variable) {
  r2 <- get_r2(data[,variable], data$winpercent)
  r2_text <- glue("R² = {format(round(r2, 3), nsmall = 3)}")
  
  
  plot <- ggplot(data=candy_data, aes(x=data[,variable], y=winpercent)) +
    labs(
      title = glue('Effect of {variable} on win percentage'),
      x = variable
    ) +
    geom_point() +
    geom_smooth(method=lm) +
    theme(
      legend.position="none",
      plot.title = element_text(color = "#0099f8", size = 12, face = "bold", hjust = 0.5),
    )
  
  plot <- plot + annotate(
    geom = "text",
    x = -Inf,
    y = Inf,      
    label = r2_text,
    hjust = -0.1,       
    vjust = 1.5,      
    size = 6,       
    color = "black"
  )
  
  return (plot)
}

plot_all_numerical_scatterplots_improved <- function(data) {
  sugar_plot <- plot_numeric_scatterplot_improved(data, "sugarpercent")
  price_plot <- plot_numeric_scatterplot_improved(data, "pricepercent")
  
  
  grid.arrange(sugar_plot, price_plot, nrow = 1)
}

#+ fig.height = 10, fig.width = 20
plot_all_numerical_scatterplots_improved(candy_data)

#' ### 1.2.2 Insights
#' From the above two scatter plots, even though we can see a slight positive correlation with `winpercent` for each of the two variables, they are quite
#' insignificant. Therefore, we can conclude that thereis no significant correlation present.
#'


#' \newpage
#'

#' # TASK 2: Bank Churn

#' ## 2.1 Exploratory Data Analysis
bank_churn_data <- read.csv("~/CMM703/Bank_Churn.csv")

#' After loading the dataset, we can view a summary of all the variables
summary(bank_churn_data)
#' However, it would be much easier to visualize the data through plots
#'

#' ### 2.1.1 Visualizing numerical data
#' We can visualize numerical data using histograms and box plots. 
plot_histogram <- function(data, variable) {
  mean = mean(data[,variable])
  sd = sd(data[,variable])
  summary_text = glue("Mean = {format(round(mean, 3), nsmall = 3)}\nSD = {format(round(sd, 3), nsmall = 3)}")  
  
  plot <- ggplot(data, aes(x=data[,variable])) +
    geom_histogram(color="black", fill="white") +
    labs(
      title = glue('Distribution of {variable}'),
      x = variable
    ) +
    theme(
      plot.title = element_text(color = "#0099f8", size = 12, face = "bold", hjust = 0.5),
    )
  
  plot <- plot + annotate(
    geom = "text",
    x = -Inf,
    y = Inf,      
    label = summary_text,
    hjust = -0.1,       
    vjust = 1.5,      
    size = 3,       
    color = "black"
  )

  
  return (plot)
}


plot_boxplot <- function(data, variable) {
  plot <- ggplot(data=data, aes(y=data[,variable])) + 
    labs(
      title = glue('Distribution of {variable}'),
      x = variable,
      y = variable
    ) +
    geom_boxplot() +
    theme(
      plot.title = element_text(color = "#0099f8", size = 12, face = "bold", hjust = 0.5),
    )
  
  
  return (plot)
}

plot_numerical <- function(data, variable) {
  histogram <- suppressMessages(plot_histogram(data, variable))
  boxplot <- plot_boxplot(data, variable)

  grid.arrange(histogram, boxplot, nrow = 1)
}

#+ fig.height = 5, fig.width = 15
plot_numerical(bank_churn_data, "CreditScore")
#' The credit score seems to have a fairly normal distribution of values.

#+ fig.height = 5, fig.width = 15
plot_numerical(bank_churn_data, "Age")
#' The age also has a somewhat normal distribution, but with some irregularities.

#+ fig.height = 5, fig.width = 15
plot_numerical(bank_churn_data, "Tenure")
#' There appears to be no pattern to the tenure, with there being around 1000 records for each year.

#+ fig.height = 5, fig.width = 15
plot_numerical(bank_churn_data, "Balance")
#' The balance follows a normal distribution as well. However there is a peak at 0.

#+ fig.height = 5, fig.width = 15
plot_numerical(bank_churn_data, "EstimatedSalary")
#' There appears to be no pattern to the Estimated Salary as well.
#'

#' ### 2.1.2 Visualizing categorical data
#' First, we should convert all categorical columns to the correct data type.
bank_churn_data$Geography <- as.factor(bank_churn_data$Geography)
bank_churn_data$Gender <- as.factor(bank_churn_data$Gender)
bank_churn_data$NumOfProducts <- as.factor(bank_churn_data$NumOfProducts)
bank_churn_data$HasCrCard <- as.factor(bank_churn_data$HasCrCard)
bank_churn_data$IsActiveMember <- as.factor(bank_churn_data$IsActiveMember)
bank_churn_data$Exited <- as.factor(bank_churn_data$Exited)

#' Now we can plot their distribution using bar charts.
plot_bar_chart <- function(data, variable) {
  plot <- ggplot(data=data, aes(x=data[,variable])) +
    geom_bar(stat="count", fill="steelblue") +
    geom_text(stat="count", aes(label=..count..), vjust=1.6, color="white", size=2.5) +
    labs(
      title = glue('Distribution of {variable}'),
      x = variable,
      y = "count"
    ) +
    theme(
      plot.title = element_text(color = "#0099f8", size = 12, face = "bold", hjust = 0.5),
    )
  return (plot)
}

#+ fig.height = 3, fig.width = 3
plot_bar_chart(bank_churn_data, "Geography")

#+ fig.height = 3, fig.width = 3
plot_bar_chart(bank_churn_data, "Gender")

#+ fig.height = 3, fig.width = 3
plot_bar_chart(bank_churn_data, "NumOfProducts")

#+ fig.height = 3, fig.width = 3
plot_bar_chart(bank_churn_data, "HasCrCard")

#+ fig.height = 3, fig.width = 3
plot_bar_chart(bank_churn_data, "IsActiveMember")

#+ fig.height = 3, fig.width = 3
plot_bar_chart(bank_churn_data, "Exited")

#' ## 2.1.3 Identifying Numeric-Numeric Correlations
#' We can calculate the correlations between numeric variables.
numeric_only <- subset(bank_churn_data, select=c("CreditScore", "Age", "Tenure", "Balance", "EstimatedSalary"))
correlations <- cor(numeric_only)
round(correlations, 3)
ggcorrplot(correlations)
#' As we can see, there does not seem to be any significant correlations between the numerical variables
#'

#' ## 2.1.4 Identifying Categorical-Categorical Correlations
#' We can calculate the correlations between categorical variables using pairwise Chi-squared tests.
categorical_columns <- c("Geography", "Gender", "NumOfProducts", "HasCrCard", "IsActiveMember", "Exited")

pairwise_p_vals <- matrix(nrow=6, ncol=6)
pairwise_chi_square <- matrix(nrow=6, ncol=6)
rownames(pairwise_p_vals) <- categorical_columns
colnames(pairwise_p_vals) <- categorical_columns
rownames(pairwise_chi_square) <- categorical_columns
colnames(pairwise_chi_square) <- categorical_columns

for (i in 1:5) {
  for (j in (i+1):6) {
    contingency_table <- table(bank_churn_data[,categorical_columns[i]], bank_churn_data[,categorical_columns[j]])
    chi_square_test <- chisq.test(contingency_table)

    p_value <- chi_square_test$p.value
    total_chi_square <- chi_square_test$statistic
    
    pairwise_p_vals[[i, j]] <- p_value
    pairwise_p_vals[[j, i]] <- p_value
    
    pairwise_chi_square[[i, j]] <- total_chi_square
    pairwise_chi_square[[j, i]] <- total_chi_square
  }
}

#' The higher the Chi-squared value, the more significant the correlation.
pheatmap(pairwise_chi_square,
         color = colorRampPalette(rev(c("red", "orange", "yellow", "white")))(100),
         display_numbers = TRUE,
         number_format = "%.5f",
         number_color = "black",
         na_col = "grey80",   
         main = "Chi-squared (X²) Statistic",
         fontsize_number = 8, 
         border_color = "white",
         cluster_rows = FALSE,
         cluster_cols = FALSE)

#' If the p-value is less than 0.05, we can consider it significant.
pheatmap(pairwise_p_vals,
         color = colorRampPalette(rev(c("red", "orange", "yellow", "white")))(100),
         display_numbers = TRUE,
         number_format = "%.5f",
         number_color = "black",
         na_col = "grey80",   
         main = "P-values",
         fontsize_number = 8, 
         border_color = "white",
         cluster_rows = FALSE,
         cluster_cols = FALSE)

#' ## 2.1.5 Identifying Numerical-Categorical Correlations
#' We can determine the correlation between numerical and categorical variables using pairwise ANOVA tests.
categorical_columns <- c("Geography", "Gender", "NumOfProducts", "HasCrCard", "IsActiveMember", "Exited")
numeric_columns <- c("CreditScore", "Age", "Tenure", "Balance", "EstimatedSalary")

pairwise_p_vals <- matrix(nrow=6, ncol=5)
pairwise_f_vals <- matrix(nrow=6, ncol=5)
rownames(pairwise_p_vals) <- categorical_columns
colnames(pairwise_p_vals) <- numeric_columns
rownames(pairwise_f_vals) <- categorical_columns
colnames(pairwise_f_vals) <- numeric_columns


for (categorical_column in categorical_columns) {
  for (numeric_column in numeric_columns) {
    anova <- aov(reformulate(categorical_column, numeric_column), data=bank_churn_data)
    p_val <- summary(anova)[[1]][["Pr(>F)"]][1]
    f_val <- summary(anova)[[1]][["F value"]][1]
    
    pairwise_p_vals[[categorical_column, numeric_column]] <- p_val
    pairwise_f_vals[[categorical_column, numeric_column]] <- f_val
  }
}

#' The higher the f-value is, the more significant the relationship.
pheatmap(pairwise_f_vals,
         color = colorRampPalette(rev(c("red", "orange", "yellow", "white")))(100),
         display_numbers = TRUE,
         number_format = "%.5f",
         number_color = "black",
         na_col = "grey80",   
         main = "f-values",
         fontsize_number = 8, 
         border_color = "white",
         cluster_rows = FALSE,
         cluster_cols = FALSE)

#' If the p-value is less than 0.05, we can consider it significant
pheatmap(pairwise_p_vals,
         color = colorRampPalette(rev(c("red", "orange", "yellow", "white")))(100),
         display_numbers = TRUE,
         number_format = "%.5f",
         number_color = "black",
         na_col = "grey80",   
         main = "p-values",
         fontsize_number = 8, 
         border_color = "white",
         cluster_rows = FALSE,
         cluster_cols = FALSE)


#' ## 2.2 Predictive Logistic Regression Model for Churn (Exited)
#' We first split the data into training and testing datasets at an 80/20 ratio.
split <- sample.split(bank_churn_data$Exited, SplitRatio = 0.8)
train_data <- subset(bank_churn_data, split == TRUE)
test_data <- subset(bank_churn_data, split == FALSE)

#' Next we develop the model. From our earlier analysis, we determined that the following variables have the most significant impact on `Exited`  
#' - `Balance`  
#' - `NumOfProducts`  
#' - `Geography`  
#' - `Gender`  
#' - `Age`  
#' - `IsActiveMember` 
model <- glm(Exited ~ Balance + NumOfProducts + Geography + Gender + Age + IsActiveMember, data = train_data, family = binomial(link = "logit"))

#' We can view the summary of the model.
summary(model)

#' ## 2.3 Getting predictions for test dataset
#' We use the threshold value as 0.5, and make a set of prediction on our test dataset
test_probabilities <- predict(model, newdata = test_data, type = "response")
predicted_exited <- ifelse(test_probabilities > 0.5, 1, 0)

#' We can compare the predicted values against our actual values and build the confusion matrix.
actual_exited <- test_data$Exited
conf_matrix <- table(Actual = actual_exited, Predicted = predicted_exited)
print(conf_matrix)
#' Here, we see that our model seems to have an issue with misclassificaion of false values (high number of false negatives).

#' And we can plot the performance metrics to analyze the performance of our model.
TP <- conf_matrix[2, 2]
TN <- conf_matrix[1, 1]
FP <- conf_matrix[1, 2]
FN <- conf_matrix[2, 1]

accuracy <- (TP + TN) / sum(conf_matrix)
precision <- TP / (TP + FP)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)

print(glue("Accuracy: {accuracy}"))
print(glue("Precision: {precision}"))
print(glue("Sensitivity: {sensitivity}"))
print(glue("Specificity: {specificity}"))
print(glue("F1 score: {f1_score}"))
#' Even though the accuracy of the model may be high, it still does not seem to perform that well when the Churn is false (as indicated by the slow Sensitivity).

#' We can also plot the ROC curve and calculate the area under it.
roc_curve <- roc(response = actual_exited, predictor = test_probabilities)
auc_value <- auc(roc_curve)
print(glue("AUC: {round(auc_value, 4)}"))

plot(roc_curve, main = "ROC Curve", print.auc = TRUE)
abline(a=0, b=1, lty=2, col="gray")


#' ## 2.4 Predicting Tenure
#' From our earlier analysis, we can see that there are few to no variables which show significant correllation with `Tenure`. Hence we will use all variables in the dataset for our model.
tenure_model = lm(Tenure ~ CreditScore + Geography + Gender + Age + Balance + NumOfProducts + HasCrCard +  + IsActiveMember + EstimatedSalary, data=train_data)
summary(tenure_model)

#' After training the model on our training dataset, we can evaluate it against our test dataset.
test_predictions <- predict(tenure_model, newdata = test_data)
actual_tenure = test_data$Tenure 

rmse <- sqrt(mean((actual_tenure - test_predictions)^2))
print(glue("RMSE = {rmse}"))
#' The root mean squared error we obtain is 2.9, which is not great considering that this would cover over 50% of the values in `Tenure`.

rss <- sum((test_predictions - actual_tenure)^2)
tss <- sum((actual_tenure - mean(actual_tenure))^2)
rsq_test <- 1 - (rss / tss)
print(glue("R² = {round(rsq_test, 4)}"))
#' The value we obtain for R² is also very low. This indicates that this model does not perform well.  

ggplot(data = test_data, aes(x = Tenure, y = test_predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs. Predicted Tenure",
       x = "Actual Tenure",
       y = "Predicted Tenure") +
  theme(
    plot.title = element_text(color = "#0099f8", size = 12, face = "bold", hjust = 0.5),
  )
#' We can also plot our predicted values against the actual values. Here we can see that our model is mostly predicting values
#' between 4.6 and 5.4. But the complete range of values fall between 0 and 10. We can infer from this, that our model does not
#' have sufficient data to make accurate predictions.
#'


#' \newpage

#' # TASK 3: Functions
#' 

#' ## 3.1 Function to identify qualitative and quantitative variable
#' Here, we simply check for the type of each column. Integer and numeric types correspond to quantitative data, while factor and logical types
#' correspond to qualitative data. Character types are of neither type.
quantitative_or_qualitative <- function(data) {
  quantitatve <- c()
  qualitative <- c()
  
  for (column_name in names(data)){
    column_type = class(data[,column_name])

    if (column_type == "integer" || column_type == "numeric" ) {
      quantitatve <- c(quantitatve, column_name)
    } else if (column_type == "factor" || column_type == "logical") {
      qualitative <- c(qualitative, column_name)
    }
    
  }
  
  return (list(quantitatve=quantitatve, qualitative=qualitative))
}

quantitative_or_qualitative(bank_churn_data)

#' ## 3.2 Handle missing values
handle_missing_values <- function(data) {
  Mode <- function(data) {
    unique_values <- unique(data)
    unique_values[which.max(tabulate(match(data, unique_values)))]
  }
  
  for (column_name in names(data)){
    column_type = class(data[,column_name])
    
    if (column_type == "integer" || column_type == "numeric") {
      data[is.na(data[,column_name]), column_name] <- mean(data[,column_name], na.rm = TRUE)
    } else if (column_type == "factor" || column_type == "logical") {
      data[is.na(data[,column_name]), column_name] <- Mode(data[,column_name])
    }
    
  }
  
  return (data)
}

#' To test this method, we add a new row to the `bank_churn` dataset with some missing values. 
bank_churn_new <- bank_churn_data
bank_churn_new[nrow(bank_churn_new) + 1,] <- list(1, "Dias", 999, NA, "Male", NA, NA, 100, NA, 1, 1, 999, 0)

#' And we ensure all columns are of the correct type
bank_churn_new$CustomerId <- as.character(bank_churn_new$CustomerId)
bank_churn_new$Surname  <- as.character(bank_churn_new$Surname )
bank_churn_new$CreditScore <- as.integer(bank_churn_new$CreditScore)
bank_churn_new$Geography <- as.factor(bank_churn_new$Geography)
bank_churn_new$Gender <- as.factor(bank_churn_new$Gender)
bank_churn_new$Tenure <- as.integer(bank_churn_new$Tenure)
bank_churn_new$Age <- as.integer(bank_churn_new$Age)
bank_churn_new$Balance  <- as.integer(bank_churn_new$Balance )
bank_churn_new$NumOfProducts  <- as.factor(bank_churn_new$NumOfProducts)
bank_churn_new$HasCrCard  <- as.factor(bank_churn_new$HasCrCard)
bank_churn_new$IsActiveMember   <- as.factor(bank_churn_new$IsActiveMember )
bank_churn_new$EstimatedSalary   <- as.numeric(bank_churn_new$EstimatedSalary )
bank_churn_new$Exited   <- as.factor(bank_churn_new$Exited )

#' Here we can see the last row of data has NA values
tail(bank_churn_new, 3)

#' We run the function to handle missing values
bank_churn_new <- handle_missing_values(bank_churn_new)

#' And now, we can see that the missing values have been replaced by the mean or the mode.
tail(bank_churn_new, 3)

#' ## 3.3 Outlier detection
#' In order to do this, we iterate through all the columns, and identify values which are either 1.5 IQR away from Q1/Q3, 
#' or 3 standard deviations away from the mean.  
#'   
#' The possible values for method are `IQR` and `SD`.
detect_outliers <- function(data, method = "IQR") {
  outliers_list <- list()
  
  for (column_name in names(data)){
    column_type = class(data[,column_name])
    if (!(column_type == "integer" || column_type == "numeric")) {
      next
    }
    
    vals <- data[,column_name]
    
    if (method == "IQR") {
      q1 <- quantile(vals, 0.25, na.rm = TRUE)
      q3 <- quantile(vals, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5*iqr
      upper_bound <- q3 + 1.5*iqr
      
      outliers <- vals[vals < lower_bound | vals > upper_bound]
      outliers_list[[column_name]] <- outliers
    } else if (method == "SD") {
      mean <- mean(vals, na.rm = TRUE)
      sd <- sd(vals, na.rm = TRUE)
      
      lower_bound <- mean - 3*sd
      upper_bound <- mean + 3*sd
      outliers <- vals[vals < lower_bound | vals > upper_bound]
      outliers_list[[column_name]] <- outliers
    } else {
      stop("Invalid method")
    }
  }
  
  return (outliers_list)
}

#' Here we check for outliers on the `bank_churn` dataset using IQR.
detect_outliers(bank_churn_new)

#' And here we check using SD.
detect_outliers(bank_churn_new, method="SD")

#' ## 3.4 Visualize
#' In order to visualize the data, we re-use two methods used in task 2. We have two separate types of visualizations
#' for qualitative data and quantitative data.
visualize <- function(data) {
  for (column_name in names(data)){
    column_type = class(data[,column_name])

    if (column_type == "integer" || column_type == "numeric") {
      #+ fig.height = 3, fig.width = 6
      plot_numerical(data, column_name)
    } else if (column_type == "factor" || column_type == "logical") {
      #+ fig.height = 3, fig.width = 3
      print(plot_bar_chart(data, column_name))
    }
  }
}

visualize(candy_data)

#' ## 3.5 Predictive Model
#' Here, we use the same two models we used in Task 2. Depending on the data type, we use either a linear regression model or a 
#' logistic regression model. We use every variable able (except for the target variable) to predict the response. And we train/test
#' against the dataset split in an 80/20 ratio.
predictive_model <- function(data, variable) {
  
  response_str <- variable
  
  variables <- c()
  for (column_name in names(data)) {
    if (column_name == variable) {
      next
    }
    
    column_type = class(data[,column_name])
    if (column_type == "character") {
      next
    }
    
    variables <- c(variables, column_name)
  }
  variables_str <- paste(variables, collapse=" + ")
  formula_str <- paste(response_str, " ~ ", variables_str)
  formula <- as.formula(formula_str)
  
  split <- sample.split(data[,variable], SplitRatio = 0.8)
  train_data <- subset(data, split == TRUE)
  test_data <- subset(data, split == FALSE)
  
  column_type = class(data[,variable])
  if (column_type == "integer" || column_type == "numeric") {
    model <- lm(formula, data=train_data)
    summary <- summary(model)
    
    test_predictions <- predict(model, newdata = test_data)
    actual <- test_data[,variable]
    rmse <- sqrt(mean((actual - test_predictions)^2))
    
    rss <- sum((test_predictions - actual)^2)
    tss <- sum((actual - mean(actual))^2)
    rsq_test <- 1 - (rss / tss)

    pva_plot <- ggplot(data = test_data, aes(x = test_data[,variable], y = test_predictions)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      labs(title = "Actual vs. Predicted",
           x = glue("Actual"),
           y = "Predicted") +
      theme(
        plot.title = element_text(color = "#0099f8", size = 12, face = "bold", hjust = 0.5),
      )
    
    return (list(
      summary=summary,
      rmse = rmse,
      rsq_test = rsq_test,
      plot = pva_plot
    ))
    
  } else if (column_type == "factor" || column_type == "logical") {
    model <- glm(formula, data=train_data, family=binomial(link = "logit"))
    summary <- summary(model)
    
    test_probabilities <- predict(model, newdata = test_data, type = "response")
    predicted <- ifelse(test_probabilities > 0.5, 1, 0)
    
    actual <- test_data[,variable]
    conf_matrix <- table(Actual = actual, Predicted = predicted)
    
    TP <- conf_matrix[2, 2]
    TN <- conf_matrix[1, 1]
    FP <- conf_matrix[1, 2]
    FN <- conf_matrix[2, 1]
    
    accuracy <- (TP + TN) / sum(conf_matrix)
    precision <- TP / (TP + FP)
    sensitivity <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)
    
    roc_curve <- roc(response = actual, predictor = test_probabilities)
    roc_plot <- plot(roc_curve, main = "ROC Curve", print.auc = TRUE)
        
    return(list(
      summary=summary,
      conf_matrix=conf_matrix,
      accuracy=accuracy,
      precision=precision,
      sensitivity=sensitivity,
      specificity=specificity,
      f1_score=f1_score,
      plot=roc_plot
    ))
  } else {
    stop("Invalid response variable")
  }
}


#' For qualitative data, we print the  
#' - Summary of the model  
#' - Confusion matrix  
#' - Accuracy, precision, sensitivity, specificity, f1 score  
#' - ROC curve  
#' - AUC value  
results <- predictive_model(candy_data, "chocolate")
print(results)
plot(results$plot)

#' For quantitative data, we print the  
#' - Summary of the model  
#' - RMSE  
#' - R² value  
#' - Scatter plot of predicted vs actual values  
results <- predictive_model(candy_data, "winpercent")
print(results)

#' ## 3.6 R Shiny App
#' This is a fairly basic Shiny app with the following steps.
#' - The user uploads a file  
#' - We display the first 5 rows as well as the qualitative/quantitative variables  
#' - The user can click on a button to calculate and view the outliers  
#' - The user can click on a button to visualize the data  
#' - The user can select a variable, and click on a button to run a model and view metrics/plots.  
#'   
#' You can view a screenshot of the application [here](https://github.com/kaneeldias/cmm703-data-analysis-assignment/blob/main/shiny_app_screenshot.pdf).
ui <- fluidPage(
  titlePanel("Analyze dataset"),
  fileInput(inputId = "upload", label = "Upload dataset", accept = "text/csv"),
  hr(),
  tableOutput("contents"),
  
  conditionalPanel(
    condition = "output.contents",
    textOutput("quantitative_vars"),
    textOutput("qualitative_vars"),

    hr(),
    selectInput(inputId = "outlier_removal_method", label = "Select outlier removal method", choices = c("IQR", "SD")),
    actionButton("detect_outliers", "Detect outliers"),
    verbatimTextOutput("outliers"),
    
    hr(),
    actionButton("visualize", "Visualize"),
    verbatimTextOutput("visualization"),
    
    hr(),
    uiOutput("variable_select_ui"),
    actionButton("run_model", "Run Model"),
    verbatimTextOutput("model"),
    plotOutput("plot")
    
  ),
  
  conditionalPanel(
    condition = "output.outliers",
  )
)

server <- function(input, output) {
  reactive_dataset  <- reactive({
    req(input$upload)

    tryCatch(
      {
        dataset <- read.csv(input$upload$datapath)
        for (column_name in names(dataset)){
          count = length(unique(dataset[,column_name]))
          if (count <= 3) {
            dataset[,column_name] <- as.factor(dataset[,column_name])
            next
          }
        }
        
        dataset <- handle_missing_values(dataset)
        return (dataset)
      },
      error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
        stop(safeError(e))
      }
    )
  })
  
  output$contents <- renderTable({
    dataset <- reactive_dataset()
    req(dataset)
    
    return (head(dataset))
  })
  
  output$quantitative_vars <- renderText({
    dataset <- reactive_dataset()
    req(dataset)
    
    variable_types <- quantitative_or_qualitative(dataset)
    glue("Quantitative variables: {paste(variable_types$quantitatve, collapse=', ')}")
  })
  
  output$qualitative_vars <- renderText({
    dataset <- reactive_dataset()
    req(dataset)
    
    variable_types <- quantitative_or_qualitative(dataset)
    glue("Qualitative variables: {paste(variable_types$qualitative, collapse=', ')}")
  })
  
  output$variable_select_ui <- renderUI({
    dataset <- reactive_dataset()
    req(dataset)
    
    variables <- colnames(dataset)
    selectInput("variable_select", "Select Variable:", choices = variables)
  })
  
  outlier_results <- eventReactive(input$detect_outliers, {
    dataset <- reactive_dataset()
    req(dataset)
    
    method <- input$outlier_removal_method
    req(method)
    
    return (detect_outliers(dataset, method))
  })
  
  output$outliers <- renderPrint({
    print(outlier_results())
  })
  
  outlier_visualization_results <- eventReactive(input$visualize, {
    dataset <- reactive_dataset()
    req(dataset)
    
    return (visualize(dataset))
  })
  
  output$visualization <- renderPrint({
    print(outlier_visualization_results())
  })
  
  model_results <- eventReactive(input$run_model, {
    dataset <- reactive_dataset()
    req(dataset)
    
    variable <- input$variable_select
    req(variable)
    
    print(variable)
    return (predictive_model(dataset, variable))
  })
  
  output$model <- renderPrint({
    print(model_results())
  })
  
  output$plot <- renderPlot({
    results <- model_results()
    plot(results$plot)
  })
  
}

shinyApp(ui = ui, server = server)

#' You can view a screenshot of the application [here](https://github.com/kaneeldias/cmm703-data-analysis-assignment/blob/main/shiny_app_screenshot.pdf).  
#'  
#' ![screenshot-1](shiny_app_screenshot-1.png)
#' ![screenshot-2](shiny_app_screenshot-2.png)