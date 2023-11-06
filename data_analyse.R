setwd("C:/Users/LAM HAI YEN/OneDrive/Máy tính/Study/Multivariate/Project")
library(tidyverse)
library(gridExtra)
library(corrplot)
library(kableExtra)
library(writexl)
library(FactoMineR)
library(Factoshiny)
library(factoextra)
library(cluster)

df <- read.csv(file = "data.csv")

# Table of variables
names <- colnames(df)
descriptions <- c("Academic principle (major/subject) of the student",
                  "Stress level in the past 3 months",
                  "Frequency in past 3 months of stress factor: financial issues",
                  "Frequency in past 3 months of stress factor: lack of confidence in academic performance",
                  "Frequency in past 3 months of stress factor: lack of confidence in subject or career choice")
df_info <- data.frame(names, descriptions)
colnames(df_info) <- c("Variable", "Descriptions")
write_xlsx(df_info, "variable_info.xlsx")

# Univariate distribution analysis

## Distribution of respondents by subject
subject_plot <- ggplot(data = df, aes(x = subject)) +
  labs(title="No. respondents by Subjects",
       y = "Count") + 
  theme_bw(base_size = 9) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=30,hjust=1), 
        axis.title.x = element_blank()) +
  geom_bar(fill = "darkblue")

### Labels corresponding with levels
stress_levels <- c("To a small extent", "Somewhat", "To a large extent", "Completely")
frequency_levels <- c("Never", "Almost never", "Sometimes", "Fairly often", "Very often")

## Distribution of respondents by level of stress
stress_plot <- ggplot(data = df, 
                      aes(x = fct_relevel(stress_level, stress_levels))) +
  labs(title="No. respondents by Stress Levels",
       y = "Count") + 
  theme_bw(base_size = 9) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=30,hjust=1), 
        axis.title.x = element_blank()) +
  geom_bar(fill = "darkblue")

## Distribution of respondents by frequencies for 3 stress factors
### Concerns about financial issues
fi_plot <- ggplot(data = df, 
                  aes(x = fct_relevel(financial_issues, frequency_levels))) +
  labs(title="Financial Issues", y = "Count") + 
  theme_bw(base_size = 9) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=30,hjust=1), 
        axis.title.x = element_blank()) +
  geom_bar(fill = "darkblue")

### Lack of confidence in academic performance
ap_plot <- ggplot(data = df, 
                  aes(x = fct_relevel(academic_performance, frequency_levels))) +
  labs(title="Academic Performance", y = "Count") + 
  theme_bw(base_size = 9) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=30,hjust=1), 
        axis.title.x = element_blank()) +
  geom_bar(fill = "darkblue")

### Lack of confidence in subject or career choices
scc_plot <- ggplot(data = df, 
                   aes(x = fct_relevel(subject_career_choice, frequency_levels))) +
  labs(title="Subject/Career Choice", y = "Count") + 
  theme_bw(base_size = 9) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=30,hjust=1), 
        axis.title.x = element_blank()) +
  geom_bar(fill = "darkblue")

### Combined plot of distribution by stress factors
grid.arrange(subject_plot, stress_plot, fi_plot, ap_plot, scc_plot,
             layout_matrix = rbind(c(1,1,2), c(1,1,2), c(1,1,2),
                                   c(3,4,5), c(3,4,5)))

# Bivariate analysis
### Function to read bivariate data from files and modify labels
read_bivariate_data <- function (root_folder, col_name_1, col_name_2) {
  file_name <- paste(paste(col_name_1, paste("+",col_name_2, sep = ""), 
                           sep = ""), ".csv", sep = "")
  bivariate_df <- data.frame(read.csv(file = paste(root_folder, file_name, sep = "/"), 
                                      row.names=1))
  if (nrow(bivariate_df) == 4) {
    row.names(bivariate_df) <- stress_levels
  }
  if (nrow(bivariate_df) == 5) {
    row.names(bivariate_df) <- frequency_levels
  }
  if (ncol(bivariate_df) == 4) {
    colnames(bivariate_df) <- stress_levels
  }
  if (ncol(bivariate_df) == 5) {
    colnames(bivariate_df) <- frequency_levels
  }
  return (bivariate_df)
}

### Function to calculate attration-repulsion based on bivariate data
attraction_repulsion_calculation <- function (df) {
  row_sum <- rowSums(df)
  col_sum <- colSums(df)
  theoretical_freq <- (t(t(row_sum)) %*% col_sum)/sum(df)
  att_repl_df <- data.frame(df / theoretical_freq)
  rownames(att_repl_df) <- rownames(df)
  return (att_repl_df)
}

### Function to plot attraction-repulsion matrix
att_repl_plot <- function (df) {
  upper_lim <- max(ceiling(max(df)), 4)
  corrplot(as.matrix(df), is.cor = FALSE,
           cl.pos = 'r', cl.align.text="l", tl.col = "black",
           col.lim = c(0,upper_lim), method = 'color',
           col=colorRampPalette(c("blue","white","red", 
               "red1", "red2", "red3", 
               "red4", "brown", "brown1",
               "brown2", "brown3", "brown4", "black", "black")[1:(upper_lim + 1)])(200),
           tl.cex = 0.6, cl.cex = 0.6)
}

## Plotting attraction repulsion of subject & stress levels, stress factors
col_name_1 <- "subject"
col_name_2 <- "stress_level"
root_folder <- "bivariate_data"
bivariate_df <- read_bivariate_data(root_folder, col_name_1, col_name_2)
att_repl_plot(attraction_repulsion_calculation(bivariate_df))

col_name_2 <- "financial_issues"
bivariate_df <- read_bivariate_data(root_folder, col_name_1, col_name_2)
att_repl_plot(attraction_repulsion_calculation(bivariate_df))

col_name_2 <- "academic_performance"
bivariate_df <- read_bivariate_data(root_folder, col_name_1, col_name_2)
att_repl_plot(attraction_repulsion_calculation(bivariate_df))

col_name_2 <- "subject_career_choice"
bivariate_df <- read_bivariate_data(root_folder, col_name_1, col_name_2)
att_repl_plot(attraction_repulsion_calculation(bivariate_df))

## Plotting attraction repulsion of stress levels & stress factors
col_name_1 <- "stress_level"
col_name_2 <- "financial_issues"
bivariate_df <- read_bivariate_data(root_folder, col_name_1, col_name_2)
att_repl_plot(attraction_repulsion_calculation(bivariate_df))

col_name_2 <- "academic_performance"
bivariate_df <- read_bivariate_data(root_folder, col_name_1, col_name_2)
att_repl_plot(attraction_repulsion_calculation(bivariate_df))

col_name_2 <- "subject_career_choice"
bivariate_df <- read_bivariate_data(root_folder, col_name_1, col_name_2)
att_repl_plot(attraction_repulsion_calculation(bivariate_df))

## Plotting attraction repulsion among stress factors
col_name_1 <- "financial_issues"
col_name_2 <- "academic_performance"
bivariate_df <- read_bivariate_data(root_folder, col_name_1, col_name_2)
att_repl_plot(attraction_repulsion_calculation(bivariate_df))

col_name_2 <- "subject_career_choice"
bivariate_df <- read_bivariate_data(root_folder, col_name_1, col_name_2)
att_repl_plot(attraction_repulsion_calculation(bivariate_df))

col_name_1 <- "academic_performance"
col_name_2 <- "subject_career_choice"
bivariate_df <- read_bivariate_data(root_folder, col_name_1, col_name_2)
att_repl_plot(attraction_repulsion_calculation(bivariate_df))

# Multivariate Correspondence Analysis
numeric_df <- read.csv(file = "numeric_data.csv")
colnames(numeric_df) <- c("Subject", "S", "FI", "AP", "SCC")
numeric_df[,c(2:5)] <- sapply(numeric_df[,c(2:5)], as.character)

stress.MCA<-MCA(numeric_df,ncp=20,graph=FALSE)
fviz_screeplot(stress.MCA, ncp=25, addlabels = TRUE)
options(ggrepel.max.overlaps = Inf)
grid.arrange(
  plot.MCA(stress.MCA,invisible= 'ind',
           col.var=c(1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5),
           cex=0.7,label =c('var')),
  plot.MCA(stress.MCA, choix='var',col.var=c(1,2,3,4,5), cex=0.7), ncol = 2)

grid.arrange(
  plot.MCA(stress.MCA,invisible= 'ind',
           col.var=c(1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5),
           cex=0.7,label =c('var'), axes = c(3,4)),
  plot.MCA(stress.MCA, choix='var',col.var=c(1,2,3,4,5), 
           cex=0.7, axes = c(3,4)), ncol = 2)

factor_df <- as.data.frame(unclass(numeric_df), stringsAsFactors = TRUE)
# calculate distance
stress.HCPC <- HCPC(stress.MCA, nb.clust = -1, graph = FALSE)
fviz_dend(stress.HCPC, 
          cex = 0.7,                    
          palette = "jco",              
          rect = TRUE, rect_fill = TRUE, 
          rect_border = "jco",          
          labels_track_height = 0.05)
fviz_cluster(stress.HCPC,
             repel = FALSE,           
             show.clust.cent = TRUE, 
             palette = "jco",         
             ggtheme = theme_minimal(),
             main = "Factor map", axes = c(12, 2))
plot(stress.HCPC, choice = "3D.map")

sink(file = "cluster_info.txt")
stress.HCPC$desc.axes$quanti.var
stress.HCPC$desc.var$test.chi2
stress.HCPC$desc.var$category
sink(file = NULL)
?HCPC
