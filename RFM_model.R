#                                             SANDRA RADGOWSKA
#                                     Customer Behavioral Segmentation

# 0. Packages --------------------------------------------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)
library(timereg)

install.packages("rfm")
library(rfm)

# 1. Loading data ----------------------------------------------------------------------------------------------------
df <- read_excel("OnlineRetail.xlsx")

# 2. Data structure --------------------------------------------------------------------------------------------------
dim(df)
str(df)
summary(df)

# Factor variables
df <- df %>%
  mutate(
    InvoiceNo = as.factor(InvoiceNo),
    StockCode = as.factor(StockCode),
    CustomerID = as.factor(CustomerID),
    Country = as.factor(Country)
  )

# Missing values check
apply(df, 2, function(x) any(is.na(x))) # there are missing values for CustomerID and Description
summary(df$CustomerID) # 135080 missing values - A LOT!

# Removing NA values for CustomerID from the analysis
df <- df %>% filter(!is.na(CustomerID))

# InvoiceNo beginning with C means that the invoice has been cancelled - those numbers to be removed from the analysis:
df %>% filter(substr(InvoiceNo, 1, 1) == "C") # almost 9,000 rows to be removed (cancelled invoices have negative quantity)
df <- df %>% filter(substr(InvoiceNo, 1, 1) != "C")

# Removing unnecessary variables - description, stockcode, country
df <- df %>% select(-c(Description, StockCode, Country))

# Removing old operations (just 12 past months)
today <- Sys.time()
df <- df %>% filter(InvoiceDate > "2010-12-09 12:50:00")
# 384568 obs left

# 3. RFM modelling --------------------------------------------------------------------------------------------
# a. manually -------------------------------------------------------------------------------------------------
# Adding MonetaryValue
df <- df %>%
  mutate(MonetaryValue = Quantity * UnitPrice)

# Adding Recency
df <- df %>% mutate(Recency = difftime(today, InvoiceDate, unit = "days")) 

# Adding Freqency
agg_df <- df %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = min(Recency),
    MonetaryValue = sum(MonetaryValue),
    Frequency = n()
  )

agg_df$Recency <- as.numeric(agg_df$Recency)

r <- qcut(agg_df$Recency, cuts = 4)
f <- qcut(agg_df$Frequency, cuts = 4)
m <- qcut(agg_df$MonetaryValue, cuts = 4)

transf_df <- as.data.frame(cbind(r, f, m))
table(transf_df)

# Appriopriate number of groups (clusters) choice
wssplot <- function(data, nc = 20, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:nc, wss,
    type = "b", xlab = "Number of Clusters",
    ylab = "Within groups sum of squares"
  )
}

wssplot(transf_df)

library(NbClust)
set.seed(1234)
nc <- NbClust(transf_df, min.nc = 2, max.nc = 20, method = "kmeans")
table(nc$Best.n[1, ])

# * Among all indices:                                                
#   9 proposed 2 as the best number of clusters 
# * 1 proposed 3 as the best number of clusters 
# * 6 proposed 4 as the best number of clusters 
# * 2 proposed 13 as the best number of clusters 
# * 1 proposed 16 as the best number of clusters 
# * 2 proposed 19 as the best number of clusters 
# * 3 proposed 20 as the best number of clusters 

# According to the majority rule, the best number of clusters is 2. Another good type is 4 clusters.
# Let's test both solutions.

# FIRST OPTION - 2 CLUSTERS
# Compute k-means with k = 2
set.seed(1234)
km.res <- kmeans(transf_df, 2, nstart = 25)
km.res

# K-means clustering with 2 clusters of sizes 2180, 2090
# 
# Cluster means:
#   r        f        m
# 1 3.195413 1.636697 1.669725
# 2 1.774641 3.376555 3.366029

# Segment 	Description 	R 	F 	M
# Champions 	Bought recently, buy often and spend the most 	4 – 5 	4 – 5 	4 – 5
# Loyal Customers 	Spend good money. Responsive to promotions 	2 – 5 	3 – 5 	3 – 5
# Potential Loyalist 	Recent customers, spent good amount, bought more than once 	3 – 5 	1 – 3 	1 – 3
# New Customers 	Bought more recently, but not often 	4 – 5 	<= 1 	<= 1
# Promising 	Recent shoppers, but haven’t spent much 	3 – 4 	<= 1 	<= 1
# Need Attention 	Above average recency, frequency & monetary values 	2 – 3 	2 – 3 	2 – 3
# About To Sleep 	Below average recency, frequency & monetary values 	2 – 3 	<= 2 	<= 2
# At Risk 	Spent big money, purchased often but long time ago 	<= 2 	2 – 5 	2 – 5
# Can’t Lose Them 	Made big purchases and often, but long time ago 	<= 1 	4 – 5 	4 – 5
# Hibernating 	Low spenders, low frequency, purchased long time ago 	1 – 2 	1 – 2 	1 – 2
# Lost 	Lowest recency, frequency & monetary scores 	<= 2 	<= 2 	<= 2

# According to the table we can divide customers into:
# 1st cluster: Potential Loyalist 	Recent customers, spent good amount, bought more than once
# 2 cluster: At risk customers

# It’s possible to compute the mean of each variables by clusters using the original data:
aggregate(agg_df, by = list(cluster = km.res$cluster), mean) %>% select(-CustomerID)

# 2 OPTION: 4 clusters
set.seed(1234)
km.res <- kmeans(transf_df, 4, nstart = 25)
km.res

# K-means clustering with 4 clusters of sizes 775, 1336, 1360, 799
# 
# Cluster means:
#   r        f        m
# 1 3.299355 3.002581 3.153548
# 2 1.424401 3.615269 3.583832
# 3 3.614706 1.461029 1.478676
# 4 1.625782 1.853567 1.792240

# According to the table we can divide customers into:
# 1st cluster: Loyal Customers 	Spend good money. Responsive to promotions
# 2nd cluster: At Risk 	Spent big money, purchased often but long time ago
# 3rd cluster: Potential Loyalist 	Recent customers, spent good amount, bought more than once
# 4th cluster: Hibernating 	Low spenders, low frequency, purchased long time ago

# It’s possible to compute the mean of each variables by clusters using the original data:
aggregate(agg_df, by = list(cluster = km.res$cluster), mean) %>% select(-CustomerID)


# b. with rfm package ------------------------------------------------------------------------------------------
str(df)
rfm_data <- df %>% select(InvoiceNo, InvoiceDate, CustomerID, UnitPrice, Quantity)

rfm_data <- rfm_data %>%
  group_by(CustomerID) %>%
  mutate(Amount = Quantity * UnitPrice) %>%
  select(-c(InvoiceNo, Quantity, UnitPrice))

# Customer segmentation
rfm_result <- rfm_table_order(rfm_data, CustomerID, InvoiceDate, Amount, today)
rfm_result

segment_names <- c(
  "Champions", "Loyal Customers", "Potential Loyalist",
  "New Customers", "Promising", "Need Attention", "About To Sleep",
  "At Risk", "Can't Lose Them", "Lost"
)

recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

segments <- rfm_segment(
  rfm_result, segment_names, recency_lower, recency_upper,
  frequency_lower, frequency_upper, monetary_lower, monetary_upper
)

segments %>%
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segment = segment, Count = n)

#Median Recency
rfm_plot_median_recency(segments)

#Median Frequency
rfm_plot_median_frequency(segments)

#Median Monetary Value
rfm_plot_median_monetary(segments)

#Heat map
rfm_heatmap(rfm_result)

#Bar chart
rfm_bar_chart(rfm_result)

#Histogram
rfm_histograms(rfm_result)

#Customers by orders
rfm_order_dist(rfm_result)

#Scatter plots
rfm_rm_plot(rfm_result)

#Frequency vs Monetary Value
rfm_fm_plot(rfm_result)

#Recency vs Frequency 
rfm_rf_plot(rfm_result)
