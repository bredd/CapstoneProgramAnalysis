source("LoadData.r")
source("AnalysisFunctions.r")
source("ReportCaeComparison.r")

# Add derivative columns
sample$hasCapstone <- ifelse(sample$Capstone.CourseId == "None", "No", "Yes")
sample$hasCyberProg <- ifelse(sample$Cybersecurity.Program == "None", "No", "Yes")
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

# CAE-CD xt Select Programs
cat("\\subsection{tab:programs-cyberprog-caecd}\n")

counts.caecd = CountColumn(sample, "CAE-CD")[c("Yes", "No")];
ct.caecd.prog <- CrosstabMultivalued(sample, "CAE-CD", "Cybersecurity.Program");
ct.caecd.select <- ct.caecd.prog[,  c("None", "BS", "MS", "Minor", "Certificate", "Concentration", "IndustryCert")];
ct.caecd.select <- InvertColumn(counts.caecd, ct.caecd.select, "None", "Any");
ct.caecd.select <- RenameColumn(ct.caecd.select, "IndustryCert", "Industry Certification");
ReportCorrelationToLatexTransposed(counts.caecd, ct.caecd.select, label="Cybersecurity Program", colLabel="CAE-CD", sumLabel="Available", chiTest="bycol");

cat("\\subsection{tab:programs-cybercourse}\n")

# Cyber Course xt HasProgram, CAE-CD
counts.cyberCourse = CountColumn(sample, "Security.Course.in.Program")[c("Required", "Elective", "None")];
ct.cyberCourse.cyberProg = CrosstabColumns(sample, "Security.Course.in.Program", "hasCyberProg");
ct.cyberCourse.cae = CrosstabColumns(sample, "Security.Course.in.Program", "CAE-CD");
ct.cyberCourse = as.table(cbind(ct.cyberCourse.cyberProg[, "Yes"], ct.cyberCourse.cae[, "Yes"]));
colnames(ct.cyberCourse) <- c("Has Cyber Course", "CAE-CD");
ReportCorrelationToLatex(counts.cyberCourse, ct.cyberCourse, chiTest="bycol");

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