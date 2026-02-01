
withwarn.chisq.test <- function(x, ...) {
    warned <- FALSE;
    result <- withCallingHandlers(
        chisq.test(x, ...),
        warning = function(w) {
            warned <<- TRUE;
            invokeRestart("muffleWarning");
        }
    );
    result$warn <- warned;
    return(result);
}

escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", x)
}

# Take one multivalued columns and generate new columns for each distinct value
ExpandMultivaluedColumn <- function(df, colname) {
    src_col <- df[[colname]];
    new_col_names <- unique(unlist(strsplit(src_col, "\\s*;\\s*")));
    new_col_names <- new_col_names[!is.na(new_col_names)]

    for (col_name in new_col_names) {
        patt <- paste0("(^|;)", escape_regex(col_name), "($|;)");
        df[[col_name]] <- ifelse(grepl(patt, src_col), "Yes", "No");
    }
    return(df);
}

DropColumn <- function(tab, colname) {
    return(tab[, colnames(tab) != colname]);
}

RenameColumn <- function(tab, oldName, newName) {
    colnames(tab)[colnames(tab) == oldName] <- newName;
    return(tab);
}

ReorderAndRenameCols <- function(tab, oldnames, newnames) {
    tab <- tab[, oldnames];
    colnames(tab) <- newnames;
    tab;
}

InvertColumn <- function(counts, tab, oldColname, newColname) {
    tab <- tab[rownames(counts),]; # Get the rows in the same order as the counts
    tab[, oldColname] <- counts - tab[, oldColname]; # Substitute the difference
    colnames(tab)[colnames(tab) == oldColname] <- newColname; # Rename the column
    tab;
}

# Generate a count table on one column
CountColumn <- function(df, col) {
    return(table(df[[col]]));
}

CountMultivaluedColumn <- function(df, col) {
    c <- df[[col]]
    c <- c[!is.na(x)]
    parts <- strsplit(x, "\\s*;\\s*")
    values <- unlist(parts)
    values <- values[nzchar(values)]
    return (table(values));
}

# Generate a contingency table on two columns
CrosstabColumns <- function(df, col1, col2) {
    c1 <- df[[col1]]
    c2 <- df[[col2]]
    return(table(c1, c2));
}

CrosstabMultivalued <- function(df, col1, col2_multi) {
    x <- df[[col1]];
    y <- df[[col2_multi]];

    keep <- !is.na(y);
    x <- x[keep];
    y <- y[keep];

    parts <- strsplit(y, "\\s*;\\s*");

    df2 <- data.frame(
    single = rep(x, lengths(parts)),
    value = unlist(parts),
    stringsAsFactors = FALSE
    );

    df2 <- df2[nzchar(df2$value), ];
    return (table(df2$single, df2$value));
}

MakeSimpleTable <- function(labels, counts) {
    tbl <- table(factor(labels));
    tbl[] <- counts;
    return(tbl);
}

AddAllRow <- function(tab) {
    if (length(dim(tab)) != 2) {
         stop("Input must be a 2‑dimensional table.")
    }
    totals <- colSums(tab);
    return (rbind(tab, All = totals));
}

AddFractionColumns <- function(tab) {
    # Get just the first, "Count" column.
    counts <- tab[,1];

    # Create a new data frame with just the count column
    out <- data.frame(value = counts, row.names=rownames(tab));
    colnames(out) <- colnames(tab)[1];

    for (i in 2:ncol(tab)) {
        # Fraction column
        colname <- paste0("f_", colnames(tab)[i]);
        out[[colname]] <- tab[,i] / counts;

        # Count column
        colname <- paste0("c_", colnames(tab)[i]);
        out[[colname]] <- tab[,i];
    }
    
    return(as.matrix(out));
}

PrintTableAsLatex <- function(tab) {
    # Column names drive the types and labeling of the rest of he output.
    names <- colnames(tab);

    # Helper function
    isCompound <- function(n) {
        if (i >= length(names)) return(FALSE);
        name = names[i];
        return (substr(name, 1, 2) == "f_"
            && substr(names[i+1], 1, 2) == "c_"
            && substr(name, 3, 50) == substr(names[i+1], 3, 50));
    } 

    # Begin the tabular
    cat("\n");
    cat("\\begin{tabular}{l");
    i <- 1;
    while (i <= length(names)) {
        cat(" r");
        i <- i + ifelse(isCompound(i), 2, 1);
    }
    cat("}\n");

    cat("\\toprule\n");

    # Write the column labels
    cat("~");
    i <- 1;
    while (i <= length(names)) {
        name <- names[i]
        if (isCompound(i)) {
            cat(" & \\multicolumn{1}{c}{\\textbf{", substr(name, 3, 50), "}}");
            i <- i+2;
        }
        else {
            cat(" & \\multicolumn{1}{c}{\\textbf{", name, "}}");
            i <- i+1;
        }
    }
    cat(" \\\\\n");

    cat("\\midrule\n");

    # Write the data
    row.names = rownames(tab);
    for (j in seq_len(nrow(tab))) {
        rowname <- row.names[j];
        if (rowname == "ZPUI" || rowname == "Z") rowname <- "PUI";
        if (rowname == "All") cat("\\midrule\n");
        cat(rowname);
        i <- 1;
        while (i <= length(names)) {
            name <- names[i];
            cat(" & ");
            if (isCompound(i)) {
                cat(round(tab[j, i] * 100), "\\% ", sep="");
                count = round(tab[j, i+1]); # Round is probably not necessary. Just being thorough.
                if (count < 10) {
                    cat("\\phantom{00}");
                }
                else if (count < 100) {
                    cat("\\phantom{0}");
                }
                cat("(", count, ")", sep="");
                i <- i+2;
            }
            else {
                cat(tab[j, i]);
                i <- i+1;
            }
        }
        cat("\\\\\n");
    }

    cat("\\bottomrule\n");
    cat("\\end{tabular}\n");
    cat("\n");
}

# Helper function
reportPercentAndValue <- function(sum, value) {
    cat(" & ", round(value / sum * 100), "\\% ", sep="");
    if (value < 10) {
        cat("\\phantom{00}");
    }
    else if (value < 100) {
        cat("\\phantom{0}");
    }
    cat("(", value, ")", sep="");
}

reportChiSq <- function(chisq) {
    cat(" & ");
    if (chisq$warn) {
        cat("~");
        return();
    }
    p <- chisq$p.value;
    cat(format(round(p, 3), nsmall=3));
    if (p < 0.001) {
        cat("***");
    }
    else if (p < 0.01) {
        cat("**\\phantom{*}");
    }
    else if (p < 0.05) {
        cat("*\\phantom{**}");
    }
    else {
        cat("\\phantom{***}");
    }
}

# chiTest can be "comp" for comprehensive, or "bycol" for by column.
ReportCorrelationToLatex <- function(counts, tab, label="~", chiTest = "") {

    # Put the rows in the table in the same order as the rows in the counts (sometimes they differ)
    tab = tab[rownames(counts),]

    # Column names drive the types and labeling of the rest of he output.
    col.names <- colnames(tab);

    # Begin the tabular
    cat("\n");
    cat("\\begin{tabular}{l r");
    for (i in 1:length(col.names)) {
        cat(" r");
    }
    cat("}\n");

    cat("\\toprule\n");

    # Write the column labels
    cat(label, " & \\multicolumn{1}{c}{\\textbf{Count}}", sep="");
    for (name in col.names) {
        cat(" & \\multicolumn{1}{c}{\\textbf{", name, "}}", sep="");
    }
    cat(" \\\\\n");

    cat("\\midrule\n");

    # Write the data
    row.names = rownames(counts);
    for (j in seq_len(nrow(tab))) {
        rowname <- row.names[j];
        if (rowname == "ZPUI" || rowname == "Z") rowname <- "PUI";
        cat(rowname);

        cat(" & ", counts[j], sep="");

        for (i in seq_len(length(col.names))) {
            reportPercentAndValue(counts[j], tab[j, i]);
        }
        cat("\\\\\n");
    }

    cat("\\midrule\n");

    all.sum <- sum(counts);
    cat("All & ", all.sum, sep="");
    for (i in seq_len(length(col.names))) {
        reportPercentAndValue(all.sum, sum(tab[,i]));
    }
    cat("\\\\\n");

    # Do a chi squared test on each column
    if (chiTest == "bycol") {
        cat("\\midrule\n");
        cat("p & ~");
        for (i in seq_len(length(col.names))) {
            chisq <- withwarn.chisq.test(cbind(tab[,i], counts - tab[,i]));
            reportChiSq(chisq);
        }
        cat(" \\\\\n");
    }

    cat("\\bottomrule\n");

    if (chiTest == "comp") {
        # Use a chi squared test to calculate p-value and Cramer's V
        chisq <- chisq.test(tab);
        V <- sqrt(chisq$statistic / (sum(tab) * (min(dim(tab)) - 1)));
        cat("\\multicolumn{", 2+length(col.names),
            "}{r}{p=", format(round(chisq$p.value, 3), nsmall=3),
            "\\hspace{2em}V=", format(round(V, 2), nsmall=2), "}\\\\\n", sep="");
    }

    cat("\\end{tabular}\n");
    cat("\n");
}

# chiTest can be "comp" for comprehensive, or "bycol" for by column.
ReportCorrelationToLatexTransposed <- function(counts, tab, label = "~", colLabel = "", sumLabel = "Sum", countLabel="~", chiTest = "") {
    doChiTest = (chiTest == "byrow" || chiTest == "bycol");

    # Put the rows in the table in the same order as the rows in the counts (sometimes they differ)
    tab = tab[rownames(counts),]

    # Column names drive the types and labeling of the rest of he output.
    col.names <- colnames(tab);
    row.names <- rownames(counts);

    # Begin the tabular
    cat("\n");
    cat("\\begin{tabular}{l r");
    for (i in 1:length(row.names)) {
        cat(" r");
    }
    cat(" r"); # All
    if (doChiTest) {
        cat(" r");
    }
    cat("}\n");

    cat("\\toprule\n");

    # Write the column labels
    if (nzchar(colLabel)) {
        cat("~ & ~ & \\multicolumn{", length(row.names), "}{c}{\\textbf{", colLabel, "}} & ~\\\\\n");
    }
    cat("\\textbf{", label, "}", sep="");
    cat(" & \\multicolumn{1}{c}{\\textbf{", sumLabel, "}}", sep="");
    for (name in row.names) {
        if (name == "ZPUI" || name == "Z") name <- "PUI";
        cat(" & \\multicolumn{1}{c}{\\textbf{", name, "}}", sep="");
    }
    if (doChiTest) {
        cat(" & \\multicolumn{1}{c}{\\textbf{p}}");
    }
    cat(" \\\\\n");

    cat("\\midrule\n");

    # Write the "Count" row
    cat(countLabel);
    cat(" & (", sum(counts), ")", sep="");
    for (j in seq_len(length(row.names))) {
         cat(" & (", counts[j], ")", sep="");
    }
    if (doChiTest) {
        cat (" & ~");
    }
    cat(" \\\\\n"); 

    # Write the data
    all.sum <- sum(counts);
    for (i in seq_len(length(col.names))) {
        cat(col.names[i]);

        # Sum
        reportPercentAndValue(all.sum, sum(tab[,i]))

        for (j in seq_len(length(row.names))) {
            reportPercentAndValue(counts[j], tab[j, i]);
        }

        if (doChiTest) {
            chisq <- withwarn.chisq.test(cbind(tab[,i], counts - tab[,i]));
            reportChiSq(chisq);
        }

        cat("\\\\\n");
    }

    cat("\\bottomrule\n");
    cat("\\end{tabular}\n");
    cat("\n");
}

ReportAssociation <- function(df, col1, col2) {
    cat("Association between '", col1, "' and '", col2, "':\n", sep="");
    chisq <- withwarn.chisq.test(table(df[[col1]], df[[col2]]));
    if (chisq$warn) {
        cat("   Unreliable sample.\n\n");
        return();
    }
    cat("    p=", format(round(chisq$p.value, 3), nsmall=3), "\n", sep="");
    cat("    $\\chi^2_{(", chisq$parameter, ")} = ", format(round(chisq$statistic, 3), nsmall=3), ", p = ", format(round(chisq$p.value, 3), nsmall=3), "$\n", sep="");
    cat("\n");
}


# Next Steps
# * Add p-value and Cramer's V to correlation table
# * Function to reorder the columns.


#res <- ReportContingencyTable(data, "category", "hasCapstone")
#res <- DropColumn(res, "No");
#RenameColumn(res, "Yes", "HasCapstone");
#print(res)
