# Load in Data
rm(list=ls())
sink(NULL)

# Set working directory to script location (optional, comment out if running from project root)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data file path (relative to project root)
file_path <- "../data/ArticleData.xlsx"
library(readxl)

data <- read_excel(file_path, sheet="ArticleData", col_names = FALSE)

# Combine the first two rows to create column names
colnames(data) <- paste(data[1, ], data[2, ], sep = "_")

# Remove the first two rows since they have been used as column names
data <- data[-c(1, 2), ]
numeric_cols <- names(data)[grepl("_(M3LogDiff|Deflator|RealGDP|YEAR)$", names(data))]
data[, numeric_cols] <- sapply(data[, numeric_cols], as.numeric)
data_original = data

# Load necessary libraries
library(dplyr)
library(zoo)

deflator_columns <- data %>%
  select(contains("_deflator")) %>%
  names()

# Create lagged columns
data <- data %>%
  mutate(across(all_of(deflator_columns), ~lag(.), .names = "{.col}_1"))

data <- data %>%
  select(1, sort(names(data)[-1]))

#data <- na.omit(data)
#data <- data[-1, ] #drop first
#data <- data[-1, ] #drop second
#data <- data[-1, ] #drop third
#data <- data[-1, ] #drop forth


library(tidyr)
df_long <- data %>% 
  gather(key = "Country_Metric", value = "Value", -DATE_YEAR)

#df_long = na.omit(df_long)



df <- df_long %>%
  separate(Country_Metric, into = c("Country", "Metric"), sep = "_", extra = "merge")
df_wide <- df %>%
  pivot_wider(names_from = Metric, values_from = Value)
colnames(df_wide) <- c("Year", "Country", "Deflator", "Deflator_1", "M3LogDiff", "RealGDP")
print(df_wide)
df_wide = na.omit(df_wide)


deflator <- as.numeric(df_wide$Deflator)
deflator1 <- as.numeric(df_wide$Deflator_1)
m3g <- as.numeric(df_wide$M3LogDiff)

# Define variables
y = deflator 
x = m3g 
n = length(y)
z = cbind(matrix(1,n,1),deflator1)

plot(m3g,deflator)

-------------------------------------------------------------
#load functions
source("functions.R")
# set grid search paramaters
r0 = 0
r1 = 40
stp = 0.1
# estimate the model
rkc <- rkc(y,x,z,r0=r0,r1=r1,stp=stp)  
#wt2 = n*(sse0-ssemin)/ssemin
sse0 = as.numeric(rkc[3])
ssek = as.numeric(rkc[7])
#wt = 357*(sse0-ssek)/ssek # 218 -> 357


# set parameters a bootstrap for testing kink effect
b0 = rkc[1]$bols   # coefficients of linear model: null model
bc = rkc[4]$betahat  # coefficients of kink model proposed by Hansen(2017)
gammahat = as.numeric(rkc[5])
w1 = rkc[8]   # statistic for kink effect W1
level = 0.90 # significant level
boot = 100 #
-------------------------------------------------------------
testkinkC <- testkinkC(y,x,z, boot=boot, b0, bc, gammahat, w1, level=level)


-------------------------------------------------------------
library(ggplot2)

threshold <- gammahat
df <- data.frame(M3 = m3g, DEFLATOR = deflator)

# Create a scatter plot with two regression lines
plot <- ggplot(df, aes(x = M3, y = DEFLATOR)) +
  geom_point() +
  geom_smooth(data = subset(df, M3 <= threshold), method = "lm", se = FALSE, color = "blue") +
  geom_smooth(data = subset(df, M3 > threshold), method = "lm", se = FALSE, color = "red") +
  geom_vline(xintercept = threshold, linetype = "dotted", color = "green") +
  labs(title = "Scatter Plot with Two Regression Lines",
       x = "M3", y = "DEFLATOR") +
  theme_minimal()

# Get the coefficients of the regression lines
coefficients_below <- coef(lm(DEFLATOR ~ M3, data = subset(df, M3 <= threshold)))
coefficients_above <- coef(lm(DEFLATOR ~ M3, data = subset(df, M3 > threshold)))


# Add equations of the regression lines as text annotations
plot +
  geom_text(aes(x = threshold - 5, y = max(df$DEFLATOR) - 10, 
                label = paste("Below Threshold: y =", round(coefficients_below[2], 2), "x +", round(coefficients_below[1], 2))),
            color = "blue") +
  geom_text(aes(x = threshold + 5, y = max(df$DEFLATOR) - 5, 
                label = paste("Above Threshold: y =", round(coefficients_above[2], 2), "x +", round(coefficients_above[1], 2))),
            color = "red")
