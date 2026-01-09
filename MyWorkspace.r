reset <- function() {
    rm(list = setdiff(ls(envir = .GlobalEnv), c("reset")), envir = .GlobalEnv);

    assign("mywd", envir = .GlobalEnv, value = function() { setwd('C:/Users/brand/Source/bredd/CapstoneProgramAnalysis');});

    assign("exit", envir = .GlobalEnv, value = function() {q(save="no");});

    assign("h", envir = .GlobalEnv, value = function() {
        cat("mywd()       Change to working directory.\n")
        cat("exit()       Exit without saving workspace.\n")
        cat("h()          Show this help text.\n")
        cat("reset()      Reset the workspace.\n")
    });

    mywd();
    cat("Reset. wd=", getwd(), "\n")
}

reset();
setwd("C:/Users/brand/OneDrive/Documents");
save.image();
mywd();
