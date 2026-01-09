# Requires install.packages("readxl")
library("readxl")

sourceFilename <- "C:\\Users\\brand\\OneDrive\\PhD\\Studies\\Capstone Program Survey\\Capstone Programs.xlsx"

# Load a data frame
data <- read_excel(sourceFilename)
colnames(data) <- make.names(colnames(data))

# Convert columns to appropriate formats
# No longer necessary as since I switched to direct excel import instead of csv.
# data$School.Size <- as.numeric(gsub(",", "", data$School.Size))

# Add derivative columns
data$hasCapstone <- ifelse(data$Capstone.CourseId == "None", "No", "Yes")

print(colnames(data));

# Report a contingency table on two columns
ReportContingencyTable <- function(df, col1, col2) {
    c1 <- df[[col1]]
    c2 <- df[[col2]]
    tab <- table(c1, c2);

    print(tab);
}

# Print a histogram of school size
#hist(data$School.Size, breaks=15)

ReportContingencyTable(data, "category", "CAE")
#ReportContingencyTable(data, "category", "hasCapstone")

