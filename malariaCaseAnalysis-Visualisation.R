malaria_cases <- read_csv("~/R-Practice/estimate_cases.csv")
View(malaria_cases)

summary(malaria_cases)

# Load necessary libraries
library(dplyr)
install.packages("modeest")
library(modeest)

mean(malaria_cases$measuredValue, na.rm = TRUE) #Gives the overall mean
# Calculate mean, median, and mode of malaria cases by region
result <- summarise(group_by(malaria_cases, REGION), #gives results for each region
                    Mean = mean(measuredValue, na.rm = TRUE),
                    Median = median(measuredValue, na.rm = TRUE),
                    Mode = mfv(measuredValue)
                    )
# Print the result
print(result)


# Install packages if not already installed
install.packages("ggplot2")
install.packages("reshape2")  # Needed for heatmaps

# Load the packages
library(ggplot2)
library(reshape2)

# Bar plot showing the total number of malaria cases in each region
plot <- ggplot(malaria_cases, aes(x = REGIONCODE, y = measuredValue, fill = REGIONCODE)) + #The fill aesthetic is also set to Region, which means each bar will be filled with a different color based on the region.
  geom_bar(stat = "identity") + #stat = "identity": This tells ggplot2 to use the values in the y aesthetic (Cases) directly to determine the height of each bar.
  labs(title = "Total Malaria Cases by Region", x = "REGIONCODE", y = "measuredValue") +
  theme_minimal() #It provides a clean and simple look by removing most of the grid lines and background elements, making the plot easier to read.

ggsave("plot.png", plot) #Save

# Remove rows with non-finite(NA) values in 'measuredValue'
cleaned_data <- malaria_cases %>% filter(is.finite(measuredValue))

# Histogram showing the distribution of malaria cases
ggplot(cleaned_data, aes(x = measuredValue)) +
  geom_histogram(binwidth = 1000, color = "Yellow") +
  labs(title = "Distribution of Malaria Cases", x = "Number of Cases", y = "Frequency") +
  theme_minimal()

# Create the heatmap
ggplot(malaria_cases, aes(x = REGIONCODE, y = factor(measuredValue), fill = measuredValue)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap of Malaria Cases by Region", x = "REGION", y = "measuredValue") +
  theme_minimal()



# Create a cross-tabulation of the regions and sum up the estimated cases
co_occurrence <- with(malaria_cases, tapply(measuredValue, list(REGIONCODE, REGIONCODE), sum, na.rm = TRUE))

# Convert the matrix into a data frame suitable for ggplot2
melted_co_occurrence <- melt(co_occurrence, na.rm = TRUE)

# Plot the heatmap
ggplot(data = melted_co_occurrence, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "green", name = "measuredValue") +
  labs(x = "Region Codes", y = "Region Codes", title = "Heatmap of Estimated Malaria Cases by Region") +
  geom_text(aes(label = value), color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




