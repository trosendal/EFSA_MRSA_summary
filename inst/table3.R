## Table 3 - Those with undefined CC (ie NA) - OBS this table is not
## included in the chapter but is useful.
source("R/functions.R")
df_AMR <- read_AMR()
desc <- function(x) {
    tab <- sort(table(x) / 19, decreasing = TRUE)
    paste(paste0(names(tab), "(", tab, ")"), collapse = ", ")
}
tab <- df_AMR[!is.na(T) & is.na(CC_infer), .(N = .N / 19,
           desc = desc(matrix_L1)),
            by = .("spa-type" = sprintf("t%03d", as.numeric(T)),
                   Year = repYear,
                   source = speciesType)]
tab <- merge(tab[source == "animal",],
             tab[source == "food", ],
             by = c("spa-type", "Year"), all = TRUE)
tab[is.na(N.x), N.x := 0]
tab[is.na(N.y), N.y := 0]
tab[is.na(desc.x), desc.x := ""]
tab[is.na(desc.y), desc.y := ""]
tab[, sum_xy := sum(N.x, N.y), by = "spa-type"]
tab <- tab[order(sum_xy, `spa-type`, Year, decreasing = TRUE)]
tab <- tab[, c("spa-type", "Year", "desc.x", "N.x",
               "desc.y", "N.y")]
tab[, N.x := as.character(N.x)]
tab[, N.y := as.character(N.y)]
tab[N.x == "0", N.x := ""]
tab[N.y == "0", N.y := ""]
tab[, Year := as.numeric(Year)]
names(tab) <- c("spa-type", "Year", "Animals (N)", "Total",
                "Food (N)", "Total")
write.csv2(tab,
           file = "tables_and_figures/table3.csv",
           row.names = FALSE,
           quote = TRUE)
