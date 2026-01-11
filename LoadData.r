sourceFilename <- "C:/Users/brand/OneDrive/PhD/Studies/Capstone Program Survey/Capstone Programs.xlsx"

# Requires install.packages("readxl")
library("readxl")

# Load a data frame and clean up the column names
data <- read_excel(sourceFilename)
colnames(data) <- make.names(colnames(data))

# Add derivative columns
data$hasCapstone <- ifelse(data$Capstone.CourseId == "None", "No", "Yes")

print("Capstone data loaded into 'data'.");