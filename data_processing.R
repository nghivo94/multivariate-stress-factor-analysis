setwd("C:/Users/LAM HAI YEN/OneDrive/Máy tính/Study/Multivariate/Project")

library(readxl)
library(dplyr)

raw_data <- read_excel(path = "raw_data.xlsx", 
                 sheet = "Data", range = cell_rows(4:222))

df <- data.frame(raw_data[c(7, 8, 32, 33, 34)])
colnames(df) <- c("subject","stress_level", "financial_issues", 
                  "academic_performance", "subject_career_choice")

write.csv(df, file = "data.csv", row.names = FALSE)

response_to_numeric <- function (response) {
  if (response == "Never" || response == "Not at all") {
    return ("1")
  }
  if (response == "Almost never" || response == "To a small extent") {
    return ("2")
  }
  if (response == "Sometimes" || response == "Somewhat") {
    return ("3")
  }
  if (response == "Fairly often" || response == "To a large extent") {
    return ("4")
  }
  if (response == "Very often" || response == "Completely") {
    return ("5")
  }
  return (response)
}

column_transform <- function (data, column_index) {
  replace_column <- c()
  for (i in 1:nrow(data)) {
    replace_column[i] <- response_to_numeric(data[i, column_index])
  }
  return (replace_column)
}

numeric_df <- df
for (i in 1:ncol(df)) {
  numeric_df[i] <- column_transform(numeric_df, i)
}

write.csv(numeric_df, file = "numeric_data.csv", row.names = FALSE)

generate_bivariate_data <- function (df, col1, col2) {
  col1_data <- data.frame(df %>% count(.[[col1]]))
  col2_data <- data.frame(df %>% count(.[[col2]]))
  result_df <- data.frame(rep(0, times = nrow(col1_data)))
  for (i in 2:nrow(col2_data)) {
    result_df <- cbind(result_df, rep(0, times = nrow(col1_data)))
  }
  colnames(result_df) <- c(col2_data[[1]])
  row.names(result_df) <- c(col1_data[[1]])
  
  combined_data <- data.frame(df %>% group_by(.[[col1]]) %>% count(.[[col2]]))
  m <- 1
  for (i in 1:nrow(combined_data)) {
    if (i != 1 && combined_data[[1]][i] != combined_data[[1]][i - 1]) {
      m <- m + 1
    }
    result_df[m, combined_data[[2]][i]] <- combined_data[[3]][i]
  }
  return (result_df)
}

save_bivariate_data <- function (root_folder, bivariate_df, col_name1, col_name2) {
  file_name <- paste(paste(col_name1, paste("+",col_name2, sep = ""), 
                           sep = ""), ".csv", sep = "")
  write.csv(bivariate_df, paste(root_folder, file_name, sep = "/"))
}

for (i in 1:(ncol(numeric_df) - 1)) {
  for (j in (i + 1):ncol(numeric_df)) {
    bivariate_df <- generate_bivariate_data(numeric_df, i, j)
    col_name_1 <- colnames(numeric_df)[i]
    col_name_2 <- colnames(numeric_df)[j]
    save_bivariate_data("bivariate_data", bivariate_df, col_name_1, col_name_2)
  }
}
