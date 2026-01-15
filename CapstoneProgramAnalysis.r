source("LoadData.r")
source("AnalysisFunctions.r")

# print(colnames(data));

q <- CountColumn(data, "category");
z <- CrosstabColumns(data, "category", "hasCapstone");
z <- DropColumn(z, "No");
q <- cbind(Count = q, HasCapstone = z)
z <- CrosstabMultivalued(data, "category", "CAE");
q <- cbind(q, z);
q <- AddAllRow(q);
q <- AddFractionColumns(q);

print(q)

# Next step - create report functions print reports by percentages in both text and LaTeX formats.