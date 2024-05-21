library(caret)

# Read the data
df <- read.csv("~/Downloads/data/Utilities.csv")
df <- df[, c("Company", "Sales", "Fuel_Cost")]

# Standardize the data
standardizer <- preProcess(df, method = c("center", "scale"))
df <- predict(standardizer, df)

# Create a larger plotting device
png("plot.png", width = 800, height = 600)  # Adjust width and height as needed

# Create the scatter plot
plot(df$Fuel_Cost, df$Sales, asp = 1)
text(df$Fuel_Cost, df$Sales - 0.1, df$Company, asp = 1)

# Calculate distance matrix
d <- dist(df)

# Perform hierarchical clustering
model <- hclust(d, method = "complete")

# Plot the dendrogram
plot(model, labels = df$Company)

# Close the plotting device
dev.off()
