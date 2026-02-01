ReportCapstoneDetails <- function(data) {

    reportFraction <- function(label, n, d) {
        percent = (n / count) * 100;
        cat(label, " & ", round(n / d * 100), "\\% ", sep="");
        pad = 4 - floor(log10(n)) - floor(log10(d));
        if (pad > 0) {
            cat(" \\phantom{", strrep("0", pad), "}", sep="");
        }
        cat("(", n, "/", d, ")\\\\\n", sep="");
    }

    reportMatches <- function(label, colname, value) {
        n = sum(data[[colname]] == value, na.rm = TRUE);
        d = sum(!is.na(data[[colname]]));
        reportFraction(label, n, d);
    }

    reportNonMatches <- function(label, colname, value) {
        n = sum(data[[colname]] != value, na.rm = TRUE);
        d = sum(!is.na(data[[colname]]));
        reportFraction(label, n, d);
    }

    reportPredicate <- function(label, colname, pred) {
        n = sum(pred(data[[colname]]), na.rm = TRUE);
        d = sum(!is.na(data[[colname]]));
        reportFraction(label, n, d);
    }



    # Reduce the sample to just those with a capstone program
    data <- data[data$Capstone.CourseId != "None",];
    count <- nrow(data);

    cat("\\begin{tabular}{l l}\n")
    cat("\\toprule\n");
    cat("Capstone Programs & \\phantom{00\\% 000/}(", count, ")\\\\\n", sep="");
    reportMatches("Has cybersecurity prereq.", "Security.Course.in.Program", "Required");
    reportMatches("Has cybersecurity component", "Cyber.Component", "Yes");
    reportMatches("Students form teams", "Team.or.Individual", "Team");
    reportNonMatches("Team has mentor or coach", "Team.Mentor.or.Coach", "None");
    reportMatches("Projects are sponsored", "Sponsored", "Yes");
    reportPredicate("Duration: Two terms or semesters", "Duration..Weeks.", function(x) {x >= 20});
    reportPredicate("Duration: One term or semester", "Duration..Weeks.", function(x) {x >= 10 & x < 20});
    cat("\\bottomrule\n");
    cat("\\end{tabular}\n");
    cat("\n");
}