##' tableE2
##'
##' Produce Annex E Table 2: Methicillin-resistant Staphylococcus
##' aureus in food, 2023.
##'
##' @param df_prev The data object
##' @param year the year to filter
##' @param path_csv path to a csv file
##' @import data.table
##' @return path to a csv file
##' @export
tableE2 <- function(df_prev = read_prev(),
                    year =  2023,
                    path_csv = tempfile(fileext = ".csv")) {
    env <- environment()
    stopifnot(identical(length(year), 1L))
    tab1 <- df_prev[year == get("year", envir = env) &
                    source == "food",
                    .(N = sum(N), n = sum(n), prop = sum(n) / sum (N)),
                    by = .(type = matrix,
                           country = country,
                           desc = matrix_txt,
                           unit = SAMPUNIT)]
    tab1 <- tab1[order(type, country, desc, unit)]
    tab1$result <- paste0(tab1$n, " (", round(tab1$prop * 100, 1), "%)")
    tab1 <- tab1[, c(1, 2, 3, 4, 5, 8)]
    names(tab1) <- c("Food", "Country", "Production type", "Sample unit",
                     "Units tested", "Positive for MRSA (%)")
    write.csv2(tab1,
               file = path_csv,
               row.names = FALSE)
    path_csv
}
