##' figure3
##'
##' Produce data for figure 3
##'
##' @export
##' @return A table of figures
figure3 <- function() {
    df <- read_AMR()
    setDT(df)
    sum(df[!is.na(CC_infer),
           .N/19,
           .(animalclass,
             host_association,
             CC_infer %in% c("398", "CC1/CC398"))]$V1)
}
