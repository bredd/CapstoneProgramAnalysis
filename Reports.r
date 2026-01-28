source("LoadData.r")
source("AnalysisFunctions.r")

counts <- CountColumn(data, "category");

hc <- CrosstabColumns(data, "category", "hasCapstone");
hc <- hc[, c("Yes", "No")]
ReportCorrelationToLatex(counts, hc);

ct.securityCourse <- CrosstabColumns(data, "category", "Security.Course.in.Program");
ct.securityCourse <- ct.securityCourse[, c("Required", "Elective", "None")];
ReportCorrelationToLatex(counts, ct.securityCourse);

ct.cae <- CrosstabMultivalued(data, "category", "CAE");
ReportCorrelationToLatex(counts, ct.cae);

#z <- DropColumn(z, "No");
#q <- cbind(Count = q, HasCapstone = z)
#z <- CrosstabMultivalued(data, "category", "CAE");
#q <- cbind(q, z);
#q <- AddAllRow(q);
#q <- AddFractionColumns(q);

#print(q)

#q <- CountColumn(data, "category");
#z <- CrosstabColumns(data, "category", "Security.Course.in.Program");
#q <- cbind(Count = q, z);
#q <- AddAllRow(q);
#q <- AddFractionColumns(q);

#print(q)

#q <- CountColumn(data, "category");
#z <- CrosstabMultivalued(data, "category", "Cybersecurity.Program");
#q <- cbind(Count = q, z);
#q <- AddAllRow(q);
#q <- AddFractionColumns(q);

#print(q)

# Next step - create report functions print reports by percentages in both text and LaTeX formats.