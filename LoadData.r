sampleFilename <- "C:/Users/brand/OneDrive/PhD/Studies/Capstone Program Survey/Capstone Programs.xlsx"
populationFilename <- "C:/Users/brand/OneDrive/PhD/Studies/Capstone Program Survey/University Sampling.xlsx"
populationSheetName <- "BS-CS";

# Requires install.packages("readxl")
library("readxl")

# Load a data frame and clean up the column names
sample <- read_excel(sampleFilename)
colnames(sample) <- make.names(colnames(sample))

# Load the population data
pop <- read_excel(populationFilename, sheet = populationSheetName)

print("Capstone data loaded into 'sample' and 'pop'.");