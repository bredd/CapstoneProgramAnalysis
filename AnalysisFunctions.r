DropColumn <- function(tab, colname) {
    return(tab[, colnames(tab) != colname]);
}

RenameColumn <- function(tab, oldName, newName) {
    colnames(tab)[colnames(tab) == oldName] <- newName;
    return(tab);
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
        if (rowname == "ZPUI") rowname <- "PUI";
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

ReportCorrelationToLatex <- function(counts, tab) {

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
    cat("~ & \\multicolumn{1}{c}{\\textbf{Count}}");
    for (name in col.names) {
        cat(" & \\multicolumn{1}{c}{\\textbf{", name, "}}", sep="");
    }
    cat(" \\\\\n");

    cat("\\midrule\n");


    # Write the data
    row.names = rownames(tab);
    for (j in seq_len(nrow(tab))) {
        rowname <- row.names[j];
        if (rowname == "ZPUI") rowname <- "PUI";
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

    cat("\\bottomrule\n");

    # Use a chi squared test to calculate p-value and Cramer's V
    chisq <- chisq.test(tab);
    V <- sqrt(chisq$statistic / (sum(tab) * (min(dim(tab)) - 1)));
    cat("\\multicolumn{", 2+length(col.names),
        "}{r}{p=", format(round(chisq$p.value, 3), nsmall=3),
        "\\hspace{2em}V=", format(round(V, 2), nsmall=2), "}\\\\\n", sep="");

    cat("\\end{tabular}\n");
    cat("\n");
}

# Next Steps
# * Add p-value and Cramer's V to correlation table
# * Function to reorder the columns.


#res <- ReportContingencyTable(data, "category", "hasCapstone")
#res <- DropColumn(res, "No");
#RenameColumn(res, "Yes", "HasCapstone");
#print(res)
