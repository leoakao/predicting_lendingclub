# Feature Scaling
lc3 <- lc2
lc3$loan_status <- NULL

lc3<- lc3[c("annual_inc", "delinq_2yrs", "dti", "emp_length_0-2", "emp_length_10+", "emp_length_3-5",
                           "emp_length_6-10", "home_ownership_MORTGAGE", "home_ownership_OTHER",
                           "home_ownership_OWN","inq_last_6mths","int_rate","issue_dsincelc","loan_amnt",
                           "num_accts_ever_120_pd","num_actv_bc_tl","num_actv_rev_tl","open_acc","term","total_acc",
                           "verification_status_Verified","verification_status_SourceVerified")]

# Using the elbow method to find the optimal number of clusters
# 5 Clusters 
set.seed(6)

wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(lc3, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans <- kmeans(x = lc3, centers = 5)
clusterno <- kmeans$cluster
lc3 <- cbind(lc3,clusterno)

# Visualising the clusters
library(cluster)
clusplot(lc3,
         clusterno,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Component 1',
         ylab = 'Component 2')












# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = dataset, centers = 5)
y_kmeans = kmeans$cluster

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')