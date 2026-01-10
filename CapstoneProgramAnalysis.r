sourceFilename <- "C:/Users/brand/OneDrive/PhD/Studies/Capstone Program Survey/Capstone Programs.xlsx"

source("AnalysisFunctions.r")

# Requires install.packages("readxl")
library("readxl")

# Load a data frame and clean up the column names
data <- read_excel(sourceFilename)
colnames(data) <- make.names(colnames(data))

# Add derivative columns
data$hasCapstone <- ifelse(data$Capstone.CourseId == "None", "No", "Yes")

# print(colnames(data));

# Print a histogram of school size
#hist(data$School.Size, breaks=15)

q <- CountColumn(data, "category");
z <- CrosstabColumns(data, "category", "hasCapstone");
z <- DropColumn(z, "No");
q <- cbind(Count = q, HasCapstone = z)
z <- CrosstabMultivalued(data, "category", "CAE");
q <- cbind(q, z);
q <- AddAllRow(q);

print(q)

# Next step - create report functions print reports by percentages in both text and LaTeX formats.