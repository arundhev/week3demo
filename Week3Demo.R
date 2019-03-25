# This script builds the managers dataset
# and populates it with data

# Load data from previous session
column_names <- c("Date", "Country", "Gender", "Age", "Q1", "Q2", "Q3", "Q4", "Q5")

# Enter data into vectors before constructing the data frame
date_col <- c("2018-15-10", "2018-01-11", "2018-21-10", "2018-28-10", "2018-01-05")
country_col <- c("US", "US", "IRL", "IRL", "IRL")
gender_col <- c("M", "F", "F", "M", "F")
age_col <- c(32, 45, 25, 39, 99) # 99 is one of the values in the age attribute - will require recoding
q1_col <- c(5, 3, 3, 3, 2)
q2_col <- c(4, 5, 5, 3, 2)
q3_col <- c(5, 2, 5, 4, 1)
q4_col <- c(5, 5, 5, NA, 2) # NA is inserted in place of the missing data for this attribute
q5_col <- c(5, 5, 2, NA, 1)

# Construct a data frame using the data from all vectors above
managers <- data.frame(date_col, country_col, gender_col, age_col, q1_col, q2_col, q3_col, q4_col, q5_col)

# Add column names to data frame using column_names vector
colnames(managers) <- column_names

# Recode the incorrect 'age' data to NA
managers$Age[managers$Age == 99] <- NA

# Create a new attribute called AgeCat and set valuess
# in AgeCat to the following if true:
# <= 25 = Young
# >= 26 & <= 44 = Middle Aged
# >= 45 = Elderly
# We will also recode age 'NA' to Elder

managers$AgeCat[managers$Age >= 45] <- "Elder"
managers$AgeCat[managers$Age >= 26 & managers$Age <= 44] <- "Middle Aged"
managers$AgeCat[managers$Age <= 25] <- "Young"
managers$AgeCat[is.na(managers$Age)] <- "Elder"

# Recode AgeCat so that is ordinal and factored with the
# order Young, Middle aged, Elder
# We'll srore the ordinal factored data in variable 'AgeCat'
AgeCat <- factor(managers$AgeCat, order = TRUE, levels = c("Young", "Middle Aged", "Elder"))

# Replace managers's AgeCat attribute with newly ordinal foctored data
managers$AgeCat <- AgeCat

# Create a new column called 'summary_col' that
# contains a summary of each row
summary_col <- managers$Q1 + managers$Q2 + managers$Q3 + managers$Q4 + managers$Q5
summary_col

# Add summary_col to the end of the data frame
managers <- data.frame(managers, summary_col)


# Calculate mean value for each row
mean_value <- rowMeans(managers[5:9])

# Add mean_value to end of managers data frame
managers <- data.frame(managers, mean_value)

# Show data frame contents
managers

# Change the name of this column to "mean value"
names(managers)[12] <- "mean value"

# Change name of summary_col to "Answer total"
names(managers)[11] <- "Answer total"

# Show 
str(managers)

#Dealing with missing data

#Removes any rows that contain NA -listwise deletion
new_data <- na.omit(managers)
new_data

#use complete cases to show rows where a full of record is available
complete_data <- complete.cases(managers)
sum(complete_data)

managers[!complete.cases(managers),]

#Finding a sum of the missing values in the age attributes
sum(is.na(managers$Age))

#use the md.pattern() function in the mice library
install.packages("mice")
library(mice)
md.pattern(managers)
install.packages("VIM")
library(VIM)

missing_values <- aggr(managers, prop = FALSE, numbers = TRUE)
summary(missing_values)

matrixplot(managers, xlab = "item", ylab = "Index", main = "identifying missing values in managers dataset")
managers
missing_values <- data.frame(is.na(managers))
head(missing_values)

#convert data frame TRUE and FALSE to 1 and 0
missing_values_logical <- data.frame(abs(is.na(managers)))
missing_values_logical

#examine each element of missing_values_logical only
#store input if values >0
correlation_matrix <- missing_values_logical[(apply(missing_values_logical, 2, sum) >0)]
correlation_matrix

cor(correlation_matrix)

#using the subset function extract all records where
#age>=35 or age < 24
#only extract Q1 and Q4


attach(managers)
new_managers <- subset(managers, managers$Age >= 35 | managers$Age < 24, select = c(Q1, Q2, Q3, Q4) )
new_managers
detach(managers)

new_managers <- subset(managers, Gender == 'M' & Age > 25, select = c(Gender:Q4) )
new_managers

my_sample <- managers[sample(1:nrow(managers), 50, replace = TRUE),]
my_sample

getwd()
