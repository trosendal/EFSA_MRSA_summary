##' tableE8_new
##'
##' Produce Annex Table 8: Methicillin-resistant Staphylococcus aureus
##' spa-type characterization, 2024.
##'
##' @param df_prev The isolate based data object
##' @param year the year to filter
##' @param inferCC should CCs be inferred from SPA
##' @param path_csv path to the output csv file
##' @import data.table
##' @return A path to a csv file
##' @export
tableE8_new <- function(df_prev = read_prev(),
                        year = 2024,
                        inferCC = TRUE,
                        path_csv = tempfile(fileext = ".csv")) {

    ## filter by year
    df_prev <- df_prev[as.numeric(REPYEAR) == year, ]

    ## inference of CC or not
    if (isTRUE(inferCC)) {
        df_prev$CC <- df_prev$CC_infer
    }

    ## Aggregate by samplingID
    df_prev <- df_prev[!is.na(T) &
                       PROGSAMPSTRATEGY != "Suspect sampling" &
                       !(SAMPCONTEXT %in% c("Clinical investigations",
                                            "Control and eradication programmes",
                                            "Outbreak investigation")),
    {
        n <- as.numeric(UNITSPOSITIVE)
        stopifnot(all(!is.na(n)))
        .(n = n,
          N = as.numeric(TOTUNITSTESTED)[1],
          year = REPYEAR,
          ST = ST,
          CC = CC,
          CC_infer = CC_infer,
          ST_infer = ST_infer,
          Host = host_association(T))
    }, by = .(samplingID,
              SPA = T,
              Category = SPECIESTYPE,
              type = MATRIX_L1,
              samp_type = SAMPTYPE,
              Country = REPCOUNTRY)
    ]
    df_prev <- df_prev[order(-Category, Country, type, samp_type, as.numeric(SPA)),
                       c("Category", "Country", "type", "samp_type", "SPA",
                         "n", "N", "ST", "CC", "CC_infer", "Host")]
    write.csv2(df_prev,
               file = "output/tab8_new.csv",
               row.names = FALSE)
}
