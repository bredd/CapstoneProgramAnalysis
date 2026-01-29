source("LoadData.r")
source("AnalysisFunctions.r")

categories <- CountColumn(sample, "category");

cat("\n\n\n");

cat("\\section{Program}\n\n");

cat("\\subsection{tab:programs-cyber-course}\n\n");

ct.securityCourse <- CrosstabColumns(sample, "category", "Security.Course.in.Program");
ct.securityCourse <- as.table(cbind(Offer = ct.securityCourse[,"Required"] + ct.securityCourse[,"Elective"], Require = ct.securityCourse[,"Required"], None = ct.securityCourse[,"None"]))
ReportCorrelationToLatex(categories, ct.securityCourse, chiTest="bycol");

cat("\\subsection{tab:programs-cae}\n\n");

ct.cae <- CrosstabMultivalued(sample, "category", "CAE");
ReportCorrelationToLatex(categories, ct.cae, chiTest="bycol");


#hasCapstone <- CrosstabColumns(sample, "category", "hasCapstone");
#hasCapstone <- hasCapstone[, c("Yes", "No")] # Reorder the columns. Yes first, then no.
#ReportCorrelationToLatex(categories, hasCapstone, includeChiTest=TRUE);

#cat("\\section{CAE}");


#cat("\\subsection{Population}");

#pop.categories = CountColumn(pop, "category");
#ct.pop.cae <- CrosstabMultivalued(pop, "category", "CAE");
#ReportCorrelationToLatex(pop.categories, ct.pop.cae);

# Compare sample CAE to population on CAE-CD
#cat("\\subsection{Pop to Sample}");

#comp.labels <- c("population", "sample");
#comp.counts <- MakeSimpleTable(comp.labels, c(nrow(pop), nrow(sample)));
#comp.tab <- as.table(rbind(as.vector(ct.pop.cae[, "CAE-CD", drop = FALSE]), as.vector(ct.cae[, "CAE-CD", drop = FALSE])));
#rownames(comp.tab) <- comp.labels;
#colnames(comp.tab) <- c("R1", "R2", "PUI");
#ReportCorrelationToLatex(comp.counts, comp.tab, includeChiTest=TRUE);

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