.onAttach <- function(lib, pkg) {
    ver <- read.dcf(file.path(lib,pkg,"DESCRIPTION"), "Version")
    bug_reports <- read.dcf(file.path(lib,pkg,"DESCRIPTION"), "BugReports")
    msg <- paste0(sprintf("uasimg (version %s)", as.character(ver)), "\n",
                  "Bug reports: ", bug_reports)
    packageStartupMessage(msg)
}
