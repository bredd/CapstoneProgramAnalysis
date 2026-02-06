# Assume that AnalysisFunctions.r has already been loaded.

ReportCyberPrograms <- function(data) {

    tags = c("BS", "MS", "Concentration", "Certificate", "Minor", "IndustryCert")
    labels = c("BS", "MS", "Concentration", "Certificate", "Minor", "Industry Certification")

    reportFraction <- function(label, n, d) {
        cat(label, " & ", sep="");
        percent = round(n / d * 100);
        pad = 1 - floor(log10(percent));
        if (pad > 0) {
            cat(" \\phantom{", strrep("0", pad), "}", sep="");
        }
        cat(percent, "\\% ", sep="");
        pad = 4 - floor(log10(n)) - floor(log10(d));
        if (pad > 0) {
            cat(" \\phantom{", strrep("0", pad), "}", sep="");
        }
        cat("(", n, "/", d, ")\\\\\n", sep="");
    }

    sample.count = nrow(data);
    t = CountMultivaluedColumn(data, "Cybersecurity.Program");

    cat("\\begin{tabular}{l l}\n")
    cat("\\toprule\n");
    cat("Institutions & \\phantom{00\\% 000/}(", sample.count, ")\\\\\n", sep="");
    row.names = rownames(t);
    for (i in seq_len(length(tags))) {
        reportFraction(labels[i], t[tags[i]], sample.count);
    }
    cat("\\bottomrule\n");
    cat("\\end{tabular}\n");
    cat("\n");
}