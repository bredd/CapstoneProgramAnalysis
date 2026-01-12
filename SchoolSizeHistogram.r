SchoolSizeHistogramA <- function(data) {

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

SchoolSizeHistogramB <- function(data) {

    r1 <- data$School.Size[data$category == "R1"]
    r2 <- data$School.Size[data$category == "R2"]
    pui <- data$School.Size[data$category == "ZPUI"]

    par(mfrow = c(3,1)) # 3 rows 1 column

    # Define common breaks
    range = c(r1, r2, pui)
    breaks <- seq(min(range), max(range), length.out = 32)

    ylim = c(0,20)

    hist(r1, breaks = breaks, ylim = ylim, main = "R1 Universities", xlab = "student body size", col = clr.sienna, xaxt = "n")
    axis(1, at = pretty(range, n = 10))
    hist(r2, breaks = breaks, ylim = ylim, main = "R2 Universities", xlab = "student body size", col = clr.teal)
    hist(pui, breaks = breaks, ylim = ylim, main = "Primarily Undergraduate Institutions (PUI)", xlab = "student body size", col = clr.blue)
}