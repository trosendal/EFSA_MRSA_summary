## Figure 1
file <- tempfile("figure1", fileext = ".pdf")
pdf(file,
    height = 12, width = 8,
    timestamp = FALSE)
EFSAMRSAsummary::figure1()
dev.off()
## stopifnot(
##     identical(
##         digest::digest(file),
##         digest::digest(system.file("extdata/tables_and_figures/figure1.pdf",
##                                    package = "EFSAMRSAsummary"))))

## Figure 2
file <- tempfile("figure2", fileext = ".pdf")
pdf(file,
    height = 12, width = 8,
    timestamp = FALSE)
EFSAMRSAsummary::figure2()
dev.off()
## stopifnot(
##     identical(
##         digest::digest(file),
##         digest::digest(system.file("extdata/tables_and_figures/figure2.pdf",
##                                    package = "EFSAMRSAsummary"))))
