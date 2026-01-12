# Principal colors - fully avoids color blindness isues
clr.red <- "#8b0000";
clr.sienna <- "#a0522d";
clr.teal <- "#008080";
clr.blue <- "#00008b";
clr.orchid <- "#9932cc";
clr.gray <- "#384f4f"

# Other colors - work, in combination with the above for all except those with severe color blindness
clr.crimson <- "#dc143c";
clr.maroon <- "#a62a64";
clr.green <- "#008000";
clr.purple <- "#663399";
clr.olive <- "#556b2f";

ColorTest <- function() {
    palette <- c(clr.red, clr.sienna, clr.teal, clr.blue, clr.orchid, clr.gray, clr.crimson, clr.maroon, clr.green, clr.purple, clr.olive, "#808080")

    par(mar = c(1,1,1,1))
    plot(1, type = "n", xlim = c(0, 6), ylim = c(0, 2), axes = FALSE, xlab = "", ylab = "")

    k <- 1
    for (row in 1:2) {
        for (col in 1:6) {
            rect(col - 1, 2 - row, col, 3 - row,
                col = palette[k], border = "black", lwd = 2)
            k <- k + 1
        }
    }  
}