# Colors selected from the 20-color palette here: https://at.mo.gov/wp-content/uploads/data-viz-accessible-color-palette.pdf
# But with a few tweaks.
# Colors tested for color-blindness compatibility here: https://pilestone.com/pages/color-blindness-simulator

# Principal colors - Avoids the majority of color blindness issues
clr.red <- "#8b0000";
clr.sienna <- "#a0522d";
clr.teal <- "#008080";
clr.blue <- "#00008b";
clr.orchid <- "#9932cc";
clr.gray <- "#384f4f"

# Other colors - In combination with the above set avoids the most common color blindness issues.
clr.crimson <- "#dc143c";
clr.maroon <- "#a62a64";
clr.green <- "#008000";
clr.purple <- "#663399";
clr.olive <- "#556b2f";
clr.azure <- "#0070e0"

# Generate a color palette
# Reviewed for color-blindness simulation at 
ColorTest <- function() {
    palette <- c(clr.red, clr.sienna, clr.teal, clr.blue, clr.orchid, clr.gray,
        clr.crimson, clr.maroon, clr.green, clr.purple, clr.olive, clr.azure)

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