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

#res <- ReportContingencyTable(data, "category", "hasCapstone")
#res <- DropColumn(res, "No");
#RenameColumn(res, "Yes", "HasCapstone");
#print(res)
