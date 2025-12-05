({Zoonosis context (L2)} = Methicillin resistant Staphylococcus aureus (MRSA):S. aureus, meticillin resistant (MRSA)) And
({Reporting year} = 2024) And
({Reporting year} (ADJUSTED_ID) >= {ValidFrom (Country status history)} (ID)) And
({Reporting year} (ADJUSTED_ID) <= {ValidTo (Country status history)} (ID)) And
({Sampling strategy} <> 3:Suspect sampling) And
({Sampling context context (L1)} <> Clinical investigations) And
(({Sample type context (L1)} = animal sample) Or
    ({Sample unit context prevalence (LAST)} = animal, herd/flock, holding, slaughter animal batch) Or
    ({Matrix context (L3)} (SPECIESTYPE) = "animal")) And
(Snapshot (ID) = 0) And
({Validity attribute} = VALID) And
({National level context} = 1) Or
({Country (Country status history)} = 258:XI:United Kingdom (Northern Ireland):, 259:XU:United Kingdom (excluding Northern Ireland):, 257:XK:Kosovo:Kosovo)

##' tableE3
##'
##' Produce Annex E Table 3: Methicillin-resistant Staphylococcus
##' aureus in food-producing animals, clinical investigations
##' excluded, 2024.
##'
##' @param df_prev The data object
##' @param year the year to filter
##' @param path_csv path to a csv file
##' @import data.table
##' @return path to a csv file
##' @export
tableE3 <- function(df_prev = read_prev(),
                    year =  2024,
                    path_csv = tempfile(fileext = ".csv")) {
    env <- environment()
    stopifnot(identical(length(year), 1L))
    nonfood <- c("Dogs", "Felidae", "Solipeds, domestic")
    tab1 <- df_prev[source == "animal" &
                    year == get("year", envir = env) &
                    !(SAMPCONTEXT %in% c("Clinical investigations", "Outbreak investigation")) &
                    !(matrix %in% nonfood),
                    .(N = sum(N), n = sum(n), prop = sum(n) / sum(N)),
                    by = .(type = matrix,
                           country = country,
                           desc = matrix_txt,
                           unit = SAMPUNIT)]
    tab1 <- tab1[order(type, country, desc, unit)]
    tab1$result <- paste0(tab1$n, " (", round(tab1$prop * 100, 1), "%)")
    tab1 <- tab1[, c(1, 2, 3, 4, 5, 8)]
    names(tab1) <- c("Animal", "Country", "Production type",
                     "Sample unit", "Units tested",
                     "Positive for MRSA (%)")
    write.csv2(tab1,
               file = path_csv,
               row.names = FALSE)
    path_csv
}


table(df_prev[, SAMPCONTEXT], df_prev[, country])

foo <- read_prev(year = as.character(1990:2030))

table(foo$SAMPCONTEXT)

df_prev[SAMPCONTEXT != "Clinical investigations", ]
