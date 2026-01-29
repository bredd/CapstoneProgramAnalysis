ReportCaeComparison <- function(samp.counts, samp.tab, pop.counts, pop.tab) {
    col.names <- colnames(pop.tab);

    # Begin the tabular
    cat("\n");
    cat("\\begin{tabular}{l");
    for (i in 1:length(col.names)) {
        cat(" r r");
    }
    cat("}\n");

    cat("\\toprule\n");

    # Write the column labels
    cat("~");
    for (name in col.names) {
        cat(" & \\multicolumn{2}{c}{\\textbf{", name, "}}", sep="");
    }
    cat(" \\\\\n");

    # Write the sub-labels
    cat("~");
    for (i in seq_len(length(col.names))) {
        cat(" & \\textbf{samp} & \\textbf{pop}");
    }
    cat(" \\\\\n");


    cat("\\midrule\n");

    # Write the data
    row.names = rownames(pop.tab);
    for (j in seq_len(nrow(pop.tab))) {
        rowname <- row.names[j];
        if (rowname == "ZPUI" || rowname == "Z") rowname <- "PUI";
        cat(rowname);

        for (i in seq_len(length(col.names))) {
            cat(" & ", round(samp.tab[j, i] / samp.counts[j] * 100), "\\% ", sep="");
            cat(" & ", round(pop.tab[j, i] / pop.counts[j] * 100), "\\% ", sep="");
        }
        cat("\\\\\n");
    }

    cat("\\midrule\n");

    pop.sum <- sum(pop.counts);
    samp.sum <- sum(samp.counts);
    cat("All");
    for (i in seq_len(length(col.names))) {
        cat(" & ", round(sum(samp.tab[, i]) / samp.sum * 100), "\\% ", sep="");
        cat(" & ", round(sum(pop.tab[, i]) / pop.sum * 100), "\\% ", sep="");
    }
    cat("\\\\\n");
    cat("\\bottomrule\n");
    cat("\\end{tabular}\n");
    cat("\n");
}