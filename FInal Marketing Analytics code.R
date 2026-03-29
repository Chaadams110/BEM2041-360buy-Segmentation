## Libraries

#install necessary packages
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("gt")
#install.packages("cluster")
#install.packages("flexclust")
#install.packages("factoextra")
#install.packages("janitor")




#load packages
library(readxl)
library(tidyverse)
library(gt)
library(cluster)
library(NbClust)
library(flexclust)
library(factoextra)
library(janitor)

## Data

data <- read_excel("C:/Users/ca624/OneDrive - University of Exeter/360buy_SurveyData.xlsx")

## Inspect the data
glimpse(data)
summary(data)


## Variable Selection and Standardisation

# Select the 5 preference variables for clustering
pref_vars <- c("CusChoice", "ConstUp", "ReplacReminder", "ProdReturn", "ProInsuCov")

# Standardise so all variables contribute equally to Euclidean distance
data_std <- scale(data[, pref_vars]) %>%
  as_tibble()

### Heirarchical Clustering
## Pairwise Distances

distance <- dist(data_std, method = "euclidean")

# Preview the first 5x5 of the distance matrix
as.matrix(distance)[1:5, 1:5]

## Run Hierarchical Clustering

set.seed(123)
hc <- hclust(distance, method = "ward.D2")
hc

## Dendrogram 

plot(hc, main = "Dendrogram – Ward's Method", xlab = "", sub = "", cex = 0.4)
rect.hclust(hc, k = 3, border = 2:4)

## Number of clusters 
set.seed(123)
NbClust(
  data   = data_std,
  min.nc = 2,
  max.nc = 8,
  index  = "all",
  method = "ward.D2"
)$Best.nc

## Majority recommend to use 3 clusters

hc3 <- cutree(hc, k = 3)

data <- data %>% mutate(hc3 = hc3)

# Cluster sizes
table(hc3)

# Z-score presentation
data_std %>%
  mutate(hc3 = factor(hc3)) %>%
  group_by(hc3) %>%
  mutate(n = n()) %>%
  summarise_all(~mean(.x)) %>%
  mutate(prop = n / sum(n)) %>%
  print(width = Inf)


## Segment Profile Plot
hc3_flex <- as.kcca(hc, data_std, k = 3)
barchart(hc3_flex)


## Enhanced Dendrogram Visualisation

# stand = FALSE because data_std is already scaled
res_hc <- hcut(data_std, k = 3, stand = FALSE)

fviz_dend(
  res_hc,
  rect     = TRUE,
  cex      = 0.5,
  k_colors = c("#00AFBB", "#2E9FDF", "#E7B800"),
  main     = "Hierarchical Clustering Dendrogram (k = 3)"
)

#### K-means Clustering method


## Number of Clusters

### Elbow Plot (Within-Cluster Sum of Squares)
fviz_nbclust(data_std, kmeans, method = "wss") +
  labs(title = "Elbow Method – Optimal Number of Clusters")

### Silhouette Method
fviz_nbclust(data_std, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method – Optimal Number of Clusters")

### NbClust Majority Vote
set.seed(123)
NbClust(
  data   = data_std,
  min.nc = 2,
  max.nc = 8,
  index  = "all",
  method = "kmeans"
)$Best.nc


## All three methods indicate that **k = 3** is the optimal number of clusters.

## Three-Cluster Solution
set.seed(123)
km <- kmeans(
  data_std,
  centers  = 3,
  iter.max = 100,
  nstart   = 100
)

km

## Cluster Centroids
round(km$centers, 2)

## Label K-Means Clusters

# Update labels below once cluster centroids have been reviewed
km3 <- factor(
  km$cluster,
  levels = c(1, 2, 3),
  labels = c("KM Segment 1", "KM Segment 2", "KM Segment 3")
)

## Segment Profile Plot
km_flex <- as.kcca(hc, data_std, k = 3)
barchart(km_flex)


## Results Comparison

# Looking at the cluster means above,
# we see that the clusters defined with the `kmeans` function are characterised
# similarly to before. We can now compare this clustering to the demographics
# as well as the hierarchical clustering.
# First, let's add the k-means clustering assignment to the raw data.
data <- data %>% mutate(km3 = km3)

### Gender
data %>%
  tabyl(km3, CusGen) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

### Education Level
data %>%
  tabyl(km3, LevEdn) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

### Income Level
data %>%
  tabyl(km3, LevIncome) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

### 360buy Account Holder
data %>%
  tabyl(km3, CusAcct) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

### Hierarchical Clustering
data %>%
  tabyl(km3, hc3) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

# Segment Profiling

## To satisfy the requirement for measures of central tendency, 
## we calculate the mean and median for each variable within each K-Means segment.
## These are computed on the **raw (unstandardised)** data so that values are 
## interpretable on the original 1–7 scale.

Task_1_2_Report <- data %>%
  group_by(km3) %>%
  summarise(across(
    c(CusChoice, ConstUp, ReplacReminder, ProdReturn, ProInsuCov,
      CusAgeYr, LevIncome),
    list(Mean = mean, Median = median),
    .names = "{.col}_{.fn}"
  ))

# Vertical layout for easy segment comparison
Task_1_2_Vertical <- Task_1_2_Report %>%
  pivot_longer(
    cols      = -km3,
    names_to  = "Variable_Metric",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from  = km3,
    values_from = Value
  )

Task_1_2_Vertical


km$centers %>%
  as_tibble(rownames = "Segment") %>%
  pivot_longer(cols = -Segment, names_to = "Variable", values_to = "Zscore") %>%
  mutate(Segment = factor(Segment, labels = c("KM Segment 1", "KM Segment 2", "KM Segment 3"))) %>%
  ggplot(aes(x = Zscore, y = Variable, fill = Segment)) +
  geom_col() +
  facet_wrap(~Segment) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "K-Means Segment Profile Plot", x = "Z-Score (deviation from mean)", y = "") +
  theme_minimal() +
  theme(legend.position = "none")

# Segment profile plot using raw Likert scale means (1-7)
data %>%
  group_by(km3) %>%
  summarise(across(c(CusChoice, ConstUp, ReplacReminder, ProdReturn, ProInsuCov), mean)) %>%
  pivot_longer(cols = -km3, names_to = "Variable", values_to = "Mean") %>%
  ggplot(aes(x = Mean, y = Variable, fill = km3)) +
  geom_col() +
  facet_wrap(~km3) +
  scale_x_continuous(limits = c(0, 7), breaks = 1:7) +
  geom_vline(xintercept = 4, linetype = "dashed", colour = "grey40") +
  labs(
    title = "Average Attribute Importance by Segment",
    x = "Mean Importance (1 = Very Unimportant, 7 = Very Important)",
    y = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Stacked bar chart showing the distribution of respondents across each segment
data %>%
  filter(!is.na(km3)) %>%
  count(km3) %>%
  mutate(prop = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = prop, fill = km3)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(km3, "\n", round(prop, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3.5, colour = "white", fontface = "bold") +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  labs(
    title = "Respondent Distribution Across K-Means Segments",
    x = "",
    y = "Percentage of Respondents (%)",
    fill = "Segment"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Account Ownership Distribution by Segment
data %>%
  filter(!is.na(km3)) %>%
  group_by(km3) %>%
  summarise(
    Has_Account = sum(CusAcct == 1),
    No_Account = sum(CusAcct == 0),
    Total = n()
  ) %>%
  pivot_longer(cols = c(Has_Account, No_Account),
               names_to = "Account_Status",
               values_to = "Count") %>%
  mutate(
    Percentage = Count / Total * 100,
    Account_Status = recode(Account_Status,
                            "Has_Account" = "Has Account",
                            "No_Account" = "No Account"
    )
  ) %>%
  ggplot(aes(x = km3, y = Percentage, fill = Account_Status)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Has Account" = "#00AFBB",
                               "No Account" = "#E7B800")) +
  scale_y_continuous(limits = c(0, 90),
                     breaks = seq(0, 90, 10)) +
  labs(
    title = "Account Ownership Distribution by Segment",
    subtitle = "Based on K-Means Cluster Analysis (k=3)",
    x = "Segment",
    y = "Percentage of Segment (%)",
    fill = "Account Status"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Formatted Mean and Median Table using gt
library(gt)

data %>%
  filter(!is.na(km3)) %>%
  group_by(km3) %>%
  summarise(across(
    c(CusChoice, ConstUp, ReplacReminder, ProdReturn, ProInsuCov),
    list(Mean = ~round(mean(.), 2),
         Median = ~median(.)),
    .names = "{.col}_{.fn}"
  )) %>%
  gt() %>%
  tab_header(
    title = "Segment Profile — Mean and Median Values",
    subtitle = "Preference Variables on 7-Point Likert Scale"
  ) %>%
  tab_spanner(label = "CusChoice",
              columns = c(CusChoice_Mean, CusChoice_Median)) %>%
  tab_spanner(label = "ConstUp",
              columns = c(ConstUp_Mean, ConstUp_Median)) %>%
  tab_spanner(label = "ReplacReminder",
              columns = c(ReplacReminder_Mean, ReplacReminder_Median)) %>%
  tab_spanner(label = "ProdReturn",
              columns = c(ProdReturn_Mean, ProdReturn_Median)) %>%
  tab_spanner(label = "ProInsuCov",
              columns = c(ProInsuCov_Mean, ProInsuCov_Median)) %>%
  cols_label(
    km3 = "Segment",
    CusChoice_Mean = "Mean", CusChoice_Median = "Median",
    ConstUp_Mean = "Mean", ConstUp_Median = "Median",
    ReplacReminder_Mean = "Mean", ReplacReminder_Median = "Median",
    ProdReturn_Mean = "Mean", ProdReturn_Median = "Median",
    ProInsuCov_Mean = "Mean", ProInsuCov_Median = "Median"
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f0f0"),
    locations = cells_column_spanners()
  ) %>%
  opt_row_striping()
