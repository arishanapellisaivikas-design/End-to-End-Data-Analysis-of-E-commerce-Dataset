rm(list = ls())
# Load Libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(corrplot)
install.packages("corrplot")

# 1. Load Dataset
data <- read.csv("C:/Users/arish/Downloads/Project 5 End-to-End Data Analysis of E-commerce/advanced_ecommerce_dataset.csv")

head(data)
colnames(data)
str(data)
summary(data)

# 2. Data Cleaning

# Remove missing values
data <- na.omit(data)

# Remove duplicate rows
data <- unique(data)

# Convert Date Column
data$Order_Date <- as.Date(data$Order_Date,
                           format="%Y-%m-%d")

# Convert numeric columns
data$Quantity <- as.numeric(data$Quantity)
data$Unit_Price <- as.numeric(data$Unit_Price)
data$Discount <- as.numeric(data$Discount)
data$Sales_Amount <- as.numeric(data$Sales_Amount)
data$Profit <- as.numeric(data$Profit)

# 3. Feature Engineering

# Extract Month and Year
data$Month <- month(data$Order_Date,
                    label = TRUE)

data$Year <- year(data$Order_Date)

# 4. Exploratory Data Analysis

# Monthly Sales Trend
monthly_sales <- data %>%
  group_by(Month) %>%
  summarise(
    Total_Sales = sum(Sales_Amount)
  )

# Regional Sales Analysis
region_sales <- data %>%
  group_by(Region) %>%
  summarise(
    Total_Sales = sum(Sales_Amount)
  )

# Category Performance
category_sales <- data %>%
  group_by(Category) %>%
  summarise(
    Total_Sales = sum(Sales_Amount),
    Total_Profit = sum(Profit)
  )

# Top Products
top_products <- data %>%
  group_by(Product_Name) %>%
  summarise(
    Total_Sales = sum(Sales_Amount)
  ) %>%
  arrange(desc(Total_Sales)) %>%
  head(10)

# Customer Type Analysis
customer_analysis <- data %>%
  group_by(Customer_Type) %>%
  summarise(
    Total_Sales = sum(Sales_Amount)
  )

# Payment Method Analysis
payment_analysis <- data %>%
  group_by(Payment_Method) %>%
  summarise(
    Total_Sales = sum(Sales_Amount)
  )

# Sales Channel Analysis
channel_analysis <- data %>%
  group_by(Sales_Channel) %>%
  summarise(
    Total_Sales = sum(Sales_Amount)
  )

# 5. Statistical Analysis

# Correlation Matrix
numeric_data <- data[, c("Quantity",
                         "Unit_Price",
                         "Discount",
                         "Sales_Amount",
                         "Profit")]

correlation_matrix <- cor(numeric_data)

print(correlation_matrix)

# Correlation Plot
corrplot(correlation_matrix,
         method = "color",
         type = "upper")

# 6. Data Visualization

# Monthly Sales Trend
plot1 <- ggplot(monthly_sales,
                aes(x = Month,
                    y = Total_Sales,
                    group = 1)) +
  
  geom_line(color = "blue",
            size = 1.2) +
  
  geom_point(size = 3,
             color = "red") +
  
  labs(
    title = "Monthly Sales Trend",
    x = "Month",
    y = "Total Sales"
  ) +
  
  theme_minimal()

# Regional Sales
plot2 <- ggplot(region_sales,
                aes(x = Region,
                    y = Total_Sales,
                    fill = Region)) +
  
  geom_bar(stat = "identity") +
  
  labs(
    title = "Regional Sales Analysis",
    x = "Region",
    y = "Total Sales"
  ) +
  
  theme_minimal()

# Category Performance
plot3 <- ggplot(category_sales,
                aes(x = reorder(Category,
                                Total_Sales),
                    y = Total_Sales,
                    fill = Category)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip() +
  
  labs(
    title = "Sales by Category",
    x = "Category",
    y = "Total Sales"
  ) +
  
  theme_minimal()

# Top Products
plot4 <- ggplot(top_products,
                aes(x = reorder(Product_Name,
                                Total_Sales),
                    y = Total_Sales,
                    fill = Product_Name)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip() +
  
  labs(
    title = "Top Selling Products",
    x = "Product",
    y = "Total Sales"
  ) +
  
  theme_minimal()

# Customer Type Analysis
plot5 <- ggplot(customer_analysis,
                aes(x = Customer_Type,
                    y = Total_Sales,
                    fill = Customer_Type)) +
  
  geom_bar(stat = "identity") +
  
  labs(
    title = "Customer Type Analysis",
    x = "Customer Type",
    y = "Total Sales"
  ) +
  
  theme_minimal()

# Payment Method Analysis
plot6 <- ggplot(payment_analysis,
                aes(x = Payment_Method,
                    y = Total_Sales,
                    fill = Payment_Method)) +
  
  geom_bar(stat = "identity") +
  
  labs(
    title = "Payment Method Analysis",
    x = "Payment Method",
    y = "Total Sales"
  ) +
  
  theme_minimal()

# Sales Channel Analysis
plot7 <- ggplot(channel_analysis,
                aes(x = Sales_Channel,
                    y = Total_Sales,
                    fill = Sales_Channel)) +
  
  geom_bar(stat = "identity") +
  
  labs(
    title = "Sales Channel Analysis",
    x = "Sales Channel",
    y = "Total Sales"
  ) +
  
  theme_minimal()

# Profit vs Sales
plot8 <- ggplot(data,
                aes(x = Sales_Amount,
                    y = Profit,
                    color = Category)) +
  
  geom_point(size = 3) +
  
  labs(
    title = "Profit vs Sales",
    x = "Sales Amount",
    y = "Profit"
  ) +
  
  theme_minimal()

# 7. Display Plots

print(plot1)
print(plot2)
print(plot3)
print(plot4)
print(plot5)
print(plot6)
print(plot7)
print(plot8)

# 8. Save Cleaned Dataset

write.csv(data,
          "cleaned_ecommerce_dataset.csv",
          row.names = FALSE)

# 9. Final Insights

cat("\nE-commerce Data Analysis Completed Successfully\n")

cat("\nKey Insights:")
cat("\n1. Monthly sales trends were identified.")
cat("\n2. Top-performing products and categories were analyzed.")
cat("\n3. Regional sales performance was evaluated.")
cat("\n4. Customer purchasing behavior was analyzed.")
cat("\n5. Payment and sales channel trends were identified.")
cat("\n6. Statistical analysis revealed relationships between sales and profit.")
cat("\n7. Visualization techniques helped generate business insights.\n")