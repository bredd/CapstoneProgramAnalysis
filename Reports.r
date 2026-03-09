source("LoadData.r")
source("AnalysisFunctions.r")
source("ReportCaeComparison.r")

# Add derivative columns
sample$hasCapstone <- ifelse(sample$Capstone.CourseId == "None", "No", "Yes")
sample$hasCyberProg <- ifelse(sample$Cybersecurity.Program == "None", "No", "Yes")
sample$hasCyberCourse <- ifelse(sample$Security.Course.in.Program == "None", "No", "Yes")
sample <- ExpandMultivaluedColumn(sample, "CAE");

categories <- CountColumn(sample, "category");

cat("\n\n\n");

cat("\\section{Program}\n");

cat("\\subsection{tab:programs-cyber-course}\n");

ct.securityCourse <- CrosstabColumns(sample, "category", "Security.Course.in.Program");
ct.securityCourse <- as.table(cbind(Required = ct.securityCourse[,"Required"], Elective = ct.securityCourse[,"Elective"], None = ct.securityCourse[,"None"]))
ReportCorrelationToLatex(categories, ct.securityCourse, chiTest="bycol");

cat("\\subsection{tab:programs-cae}\n\n");

ct.cae <- CrosstabMultivalued(sample, "category", "CAE");
ReportCorrelationToLatex(categories, ct.cae, chiTest="bycol");

cat("\\subsection{tab:programs-cae-pop}\n");

pop.categories = CountColumn(pop, "category");
ct.pop.cae <- CrosstabMultivalued(pop, "category", "CAE");
ReportCorrelationToLatex(pop.categories, ct.pop.cae, chiTest="bycol");

# Compare sample CAE to population on CAE-CD
cat("\\subsection{tab:programs-compare-cae}\n");

comp.labels <- c("population", "sample");
comp.counts <- MakeSimpleTable(comp.labels, c(nrow(pop), nrow(sample)));
comp.tab <- as.table(rbind(as.vector(ct.pop.cae[, "CAE-CD", drop = FALSE]), as.vector(ct.cae[, "CAE-CD", drop = FALSE])));
rownames(comp.tab) <- comp.labels;
colnames(comp.tab) <- c("R1", "R2", "PUI");
#ReportCorrelationToLatex(comp.counts, comp.tab, chiTest="bycol");
ReportCaeComparison(categories, ct.cae, pop.categories, ct.pop.cae);

cat("\\subsection{other:programs-correlation-cae}\n\n");

chisq = chisq.test(comp.tab);
cat("chi-square goodnes-of-fit test on CAE sample vs population: $\\chi^2_{(", chisq$parameter, ")} = ", format(round(chisq$statistic, 3), nsmall=3), ", p = ", format(round(chisq$p.value, 3), nsmall=3), "$\n\n", sep="");

# Select Programs xt Cyber Course
cat("\\subsection{tab:programs-cyberprog-cybercourse}\n")

## This report doesn't work quite right. The percentages in each column are of the total at the top, not the row at the left
## And I think the chi square tests aren't done right.
## For the report as of 2026-03-08 I only used the first column which reports the number and percentage of cybersecurity programs at the institutions.
## Then I manually checked for correlation between having any cybersecurity program and having a cybersecurity course

counts.cybercourse = CountColumn(sample, "Security.Course.in.Program")[c("Required", "Elective", "None")];
ct.cybercourse.prog <- CrosstabMultivalued(sample, "Security.Course.in.Program", "Cybersecurity.Program");
ct.cybercourse.select <- ct.cybercourse.prog[,  c("None", "BS", "MS", "Minor", "Certificate", "Concentration", "IndustryCert")];
ct.cybercourse.select <- InvertColumn(counts.cybercourse, ct.cybercourse.select, "None", "Any");
ct.cybercourse.select <- RenameColumn(ct.cybercourse.select, "IndustryCert", "Industry Certification");
ReportCorrelationToLatexTransposed(counts.cybercourse, ct.cybercourse.select, label="Cybersecurity Program", colLabel="Cybersecurity Course", sumLabel="Offered", chiTest="bycol");

# Here I look for a correlation between having a cybersecurity program and having a cybersecurity course.
# There were too few samples in one of the conditions for a reliable chi squared test so I used Fisher's exact test.
ct.prog.course <- CrosstabColumns(sample, "hasCyberProg", "hasCyberCourse");
print(fisher.test(ct.prog.course));

# CAE-CD xt Select Programs
cat("\\subsection{tab:programs-cyberprog-caecd}\n")

counts.caecd = CountColumn(sample, "CAE-CD")[c("Yes", "No")];
ct.caecd.prog <- CrosstabMultivalued(sample, "CAE-CD", "Cybersecurity.Program");
ct.caecd.select <- ct.caecd.prog[,  c("None", "BS", "MS", "Minor", "Certificate", "Concentration", "IndustryCert")];
ct.caecd.select <- InvertColumn(counts.caecd, ct.caecd.select, "None", "Any");
ct.caecd.select <- RenameColumn(ct.caecd.select, "IndustryCert", "Industry Certification");
ReportCorrelationToLatexTransposed(counts.caecd, ct.caecd.select, label="Cybersecurity Program", colLabel="CAE-CD", sumLabel="Offered", chiTest="bycol");

cat("\\subsection{tab:programs-cybercourse}\n")

ReportAssociation(sample, "Security.Course.in.Program", "hasCyberProg");
ReportAssociation(sample, "Security.Course.in.Program", "CAE-CD");
ReportAssociation(sample, "hasCyberProg", "CAE-CD");
ReportAssociation(sample, "hasCapstone", "Security.Course.in.Program");
ReportAssociation(sample, "hasCapstone", "hasCyberProg");
ReportAssociation(sample, "hasCapstone", "CAE-CD");

cat("\\subsection{tab:programs-capstone}\n")

ct.capstoneCourse <- CrosstabColumns(sample, "category", "hasCapstone");
ct.capstoneRequired <- CrosstabColumns(sample, "category", "Required.for.Degree");
ct.capstone <- as.table(cbind(ct.capstoneCourse[,"Yes"], ct.capstoneRequired[,"Yes"]));
colnames(ct.capstone) <- c("Has Capstone", "Required");
ReportCorrelationToLatex(categories, ct.capstone, chiTest="bycol");

cat("\\subsection{tab:programs-details}\n\n")

source("ReportCapstoneDetails.r");
ReportCapstoneDetails(sample);


# CAE-CD xt Select Programs
#ct.caecd.select <- ct.caecd.prog[, c("BS", "MS", "Certificate", "None")];
#ct.caecd.select <- InvertColumn(counts.caecd, ct.caecd.select, "None", "Any");
#ReportCorrelationToLatex(counts.caecd, ct.caecd.select, chiTest="bycol");

#hasCapstone <- CrosstabColumns(sample, "category", "hasCapstone");
#hasCapstone <- hasCapstone[, c("Yes", "No")] # Reorder the columns. Yes first, then no.
#ReportCorrelationToLatex(categories, hasCapstone, includeChiTest=TRUE);

#cat("\\section{CAE}");


#cat("\\subsection{Population}");



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