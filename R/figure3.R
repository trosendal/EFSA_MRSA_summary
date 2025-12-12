##' figure3
##'
##' Produce data for figure 3
##'
##' @export
##' @return A table of figures
figure3 <- function() {
    df <- read_AMR()
    df$host <- host_association(df$SPA)
    setDT(df)
    df$samplingID <- as.numeric(
        as.factor(
            paste0(df$repYear,
                   df$matrix_C,
                   df$sampUnitType,
                   df$repCountry,
                   df$sampArea,
                   df$totUnitsTested,
                   df$totUnitsPositive)))
    df <- df[, .(samplingID,
                 animalclass,
                 host_association,
                 CC_infer = CC_infer %in% c("398", "CC1/CC398"),
                 pos = totUnitsPositive)]
    df <- df[, as.numeric(pos[1]),
             by = .(samplingID, animalclass, host_association, CC_infer)]
    df[, sum(V1), by = .(animalclass, host_association, CC_infer)]
}
