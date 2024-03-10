library(readr)
library(dplyr)
library(scatterplot3d)
library(RColorBrewer)
library(ggplot2)

# Load the CSV file
used_cars <- read.csv("C:\Users\Aishu\Downloads\vehicles.csv")

# Selecting the specific columns to use 
selected_columns <- used_cars[, c("id","region", "price", "year", "manufacturer", "model", "condition", "cylinders", "fuel", "odometer", "title_status", "transmission", "VIN", "size", "type", "paint_color","state", "lat", "long", "posting_date")]

# Removing the columns that are not correct mostly having null values
selected_columns <- selected_columns[-(1:27), , drop = FALSE]

# Replacing values less than 500 in odometer with NA
selected_columns$odometer <- ifelse(selected_columns$odometer < 500, NA, selected_columns$odometer)

# Replacing values less than 1000 in price with NA
selected_columns$price <- ifelse(selected_columns$price < 1000, NA, selected_columns$price)

# Impute missing values in odometer with mean
mean_odometer <- round(mean(selected_columns$odometer, na.rm = TRUE), 0)
selected_columns$odometer[is.na(selected_columns$odometer)] <- mean_odometer

# Impute missing values in price with mean
mean_price <- round(mean(selected_columns$price, na.rm = TRUE), 0)
selected_columns$price[is.na(selected_columns$price)] <- mean_price

# Finding age of the car
current_year <- as.numeric(format(Sys.Date(), "%Y"))      # Calculated the current year
selected_columns$age <- current_year - selected_columns$year
str(selected_columns)


#Question 1
# Identify the top 10 regions based on median price
top_regions <- names(sort(tapply(selected_columns$price, selected_columns$region, median), decreasing = TRUE))[1:10]

# Filter the data for the top 10 regions
selected_columns_top10 <- selected_columns[selected_columns$region %in% top_regions, ]

# Create a bar graph for the top 10 regions
ggplot(selected_columns_top10, aes(x = factor(region, levels = top_regions), y = price)) +
  geom_bar(stat = "summary", fun = "median", fill = "darkblue") +
  labs(title = "Distribution of Car Prices in Top 10 Regions",
       x = "Region",
       y = "Median Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Question 2
#Randomly select 50 records to know about the relation of price,year and odometer
random_sample <- selected_columns[sample(nrow(selected_columns), 50), ]
scatterplot3d(random_sample$age, random_sample$price,
              xlab = "Age of the Car", ylab = "Car Price",
              main = "Bubble Plot to compare Car Price, Age, and Odometer",
              color = "blue", grid = TRUE, pch = 16, angle = 55)


#) Question 3

#What is the distribution of car conditions (e.g., excellent, good, fair) across different regions, and how does it impact the average car price?
# Group by region and condition, calculate average price
condition_price <- selected_columns %>%
  group_by(region, condition) %>%
  summarise(avg_price = mean(price, na.rm = TRUE))
# Create a bar plot showing the distribution of conditions in each region
ggplot(condition_price, aes(x = region, fill = condition)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Car Conditions Across Regions",
       x = "Region",
       y = "Proportion",
       fill = "Condition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter plot showing the relationship between condition and average price
ggplot(condition_price, aes(x = condition, y = avg_price, color = condition)) +
  geom_point(size = 3) +
  labs(title = "Impact of Car Condition on Average Price",
       x = "Condition",
       y = "Average Car Price") +
  theme_minimal()

# Question 4
# How has the number of car postings evolved over time?

selected_columns$posting_date <- as.Date(selected_columns$posting_date)

# Count the number of postings per day
posting_counts <- selected_columns %>%
  group_by(posting_date) %>%
  summarise(number_of_postings = n())

# Create a time series plot
ggplot(posting_counts, aes(x = posting_date, y = number_of_postings)) +
  geom_line(color = "blue") +
  labs(title = "Trend in Number of Car Postings Over Time",
       x = "Date",
       y = "Number of Car Postings")

# Question 5
# Violin plot using the special column called cylinders
# Ensure there are at least 100 records in your dataset
if (nrow(selected_columns) >= 100) {
  # Randomly select 100 records
  random_sample <- selected_columns[sample(nrow(selected_columns), 100, replace = TRUE), ]
  
  # Create violin plot using the selected 100 records
  ggplot(random_sample, aes(x = as.factor(cylinders), y = price)) +
    geom_violin(fill = "lightblue", color = "blue", size = 1.5, alpha = 0.7) +
    labs(title = "Car Price Distribution by Number of Cylinders",
         x = "Number of Cylinders",
         y = "Car Price") +
    theme_minimal()
} else {
  cat("Error: The dataset has fewer than 100 records.")
}



#Question 6
# Function to predict price based on user input
predict_price <- function(model) {
  # Get user input for year and odometer values
  year1 <- as.numeric(readline("Enter the car's year: "))
  odometer1 <- as.numeric(readline("Enter the car's odometer reading: "))
  
  # Create a data frame with user input
  car1 <- data.frame(year = year1, odometer = odometer1)
  
  # Predict the price
  predicted_price <- predict(model, newdata = car1)
  
  # Set negative predicted prices to 0
  predicted_price <- ifelse(predicted_price < 0, 0, predicted_price)
  
  # Print the predicted price
  print(paste("Predicted Price for the New Car:", round(predicted_price, 2)))
}
# Use the function to predict price based on user input
predict_price(linear_model)





