SchoolSizeHistogram <- function(data) {

    r1 <- data$School.Size[data$category == "R1"]
    r2 <- data$School.Size[data$category == "R2"]
    pui <- data$School.Size[data$category == "ZPUI"]

    # Define common breaks
    breaks <- seq(min(r1, r2, pui), max(r1, r2, pui), length.out = 12)

    h1 <- hist(r1, breaks = breaks, plot = FALSE)
    h2 <- hist(r2, breaks = breaks, plot = FALSE)
    hpui <- hist(pui, breaks = breaks, plot = FALSE)

    counts <- rbind(h1$counts, h2$counts, hpui$counts)

    barplot(
        counts,
        beside = TRUE,
        col = c("red", "green", "blue"),
        names.arg = round(h1$mids, 1),
        xlab = "Value",
        main = "Side-by-Side Histograms (Base R)"
    )

    legend("topright", legend = c("r1", "r2", "pui"),
        fill = c("red", "green", "blue"))
}