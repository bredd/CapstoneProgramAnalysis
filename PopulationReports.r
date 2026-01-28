sourceFilename <- "C:/Users/brand/OneDrive/PhD/Studies/Capstone Program Survey/University Sampling.xlsx"
sourceSheetName <- "BS-CS";

# Requires install.packages("readxl")
library("readxl")

source("AnalysisFunctions.r")

# Load a data frame and clean up the column names
data <- read_excel(sourceFilename, sourceSheetName)
colnames(data) <- make.names(colnames(data))

counts <- CountColumn(data, "Category");

ct.cae <- CrosstabMultivalued(data, "Category", "CAE");
ReportCorrelationToLatex(counts, ct.cae);