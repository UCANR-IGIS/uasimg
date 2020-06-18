.onAttach <- function(lib, pkg) {
    ver <- read.dcf(file.path(lib,pkg,"DESCRIPTION"), "Version")
    msg <- sprintf("uasimg (version %s)\nBug reports: https://github.com/ucanr-igis/uasimg/issues", as.character(ver))
    packageStartupMessage(msg)
}
