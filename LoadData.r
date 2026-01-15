sourceFilename <- "C:/Users/brand/OneDrive/PhD/Studies/Capstone Program Survey/Capstone Programs.xlsx"

# Requires install.packages("readxl")
library("readxl")

# Load a data frame and clean up the column names
data <- read_excel(sourceFilename)
colnames(data) <- make.names(colnames(data))

# Add derivative columns
data$hasCapstone <- ifelse(data$Capstone.CourseId == "None", "No", "Yes")

# Prepare the color palette
# https://at.mo.gov/wp-content/uploads/data-viz-accessible-color-palette.pdf

print("Capstone data loaded into 'data'.");