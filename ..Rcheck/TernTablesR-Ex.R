pkgname <- "TernTablesR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('TernTablesR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ternD")
### * ternD

flush(stderr()); flush(stdout())

### Name: ternD
### Title: Generate descriptive summary table (optionally normality-aware)
### Aliases: ternD

### ** Examples

# ternD(mtcars, consider_normality = TRUE, print_normality = TRUE)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
