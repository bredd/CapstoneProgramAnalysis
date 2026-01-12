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

# Principal colors
clr.red = "#8b0000";
clr.sienna = "#a0522d";
clr.teal = "#008080";
clr.blue = "#00008b";
clr.orchid = "#9932cc";
clr.gray = "#384f4f"

# Other colors
clr.crimson = "#dc143c";
clr.maroon = "#a62a64";
clr.green = "#008000"
clr.purple = "#663399";
clr.olive "#556b2f";

print("Capstone data loaded into 'data'.");