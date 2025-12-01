##' table1
##'
##' Table 1 - CC398 only in main chapter
##' Produce Table 1 in the main chapter
##'
##' @param df_AMR The isolate based data object
##' @param years the years to filter
##' @param path_csv path to the output csv file
##' @import data.table
##' @return A path to a csv file
##' @export
table1 <- function(df_AMR = read_AMR(),
                   path_csv = tempfile(fileext = ".csv")) {
    ## Helper to format the text in the table
    desc <- function(x) {
        tab <- sort(table(x) / 19, decreasing = TRUE)
        paste(paste0(names(tab), "(", tab, ")"), collapse = ", ")
    }
    tab <- df_AMR[!is.na(T) &
                  CC_infer %in% c("398", "CC1/CC398"), .(N = .N / 19,
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
               file = path_csv,
               row.names = FALSE,
               quote = TRUE)
    path_csv
}
