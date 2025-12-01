##' tableE9
##'
##' Produce Annex Table 9: Methicillin-resistant Staphylococcus aureus
##' spa-type characterization, 2023.
##'
##' @param df_AMR The isolate based data object
##' @param year the year to filter
##' @param path_csv path to the output csv file
##' @import data.table
##' @return A path to a csv file
##' @export
tableE9 <- function(df_AMR = read_AMR(),
                    year = 2023,
                    path_csv = tempfile(fileext = ".csv")) {
    stopifnot(identical(length(year), 1L))
    subdf <- df_AMR[df_AMR$repYear == year & !is.na(df_AMR$T), ]
    ## Helper to fix unique values in data.table
    collapse_unique <- function(x) {
        x <- unique(x[!is.na(x) & x != ""])
        if (length(x)) paste(x, collapse = ", ") else ""
    }
    subdf$spatype <- subdf$T
    iso <- subdf[, .(
        mecA_any = any(mecA == TRUE, na.rm = TRUE),
        mecC_any = any(mecC == TRUE, na.rm = TRUE),
        ST       = collapse_unique(ST),
        CC       = collapse_unique(CC),
        iCC      = collapse_unique(CC_infer),
        iST      = collapse_unique(ST_infer)
    ), by = .(speciesType, repCountry, matrix_L1, sampUnitType, spatype, LABISOLCODE)]
    ##
    tab <- iso[, .(
        n_iso   = uniqueN(LABISOLCODE), # number of isolates
        mecA_any = any(mecA_any),
        mecC_any = any(mecC_any),
        ST       = collapse_unique(ST),
        CC       = collapse_unique(CC),
        iCC      = collapse_unique(iCC),
        iST      = collapse_unique(iST)
    ), by = .(Category = speciesType,
              Country  = repCountry,
              AnimalFoodType = matrix_L1,
              SampleType     = sampUnitType,
              `spa-type`     = spatype)]
    tab[, N := sum(n_iso), by = .(Category, Country, AnimalFoodType, SampleType)]
    tab[, `No. of isolates ` := paste0(n_iso, "/", N)]
    tab[, `PVL status/ IEC genes` := "-"]
    tab[, `mec-gene` := fifelse(mecA_any & mecC_any, "mecA, mecC",
                                fifelse(mecA_any & !mecC_any, "mecA",
                                        fifelse(!mecA_any & mecC_any, "mecC", "-mecA, -mecC")))]
    tab[, `LA, CA or HA` := ""]
    setnames(tab, c("iCC","iST"), c("Inferred CC","Inferred ST/CC & type"))
    tab <- tab[order(tab$Category,
                     tab$Country,
                     tab$AnimalFoodType,
                     tab$SampleType,
                     tab$n_iso,
                     tab$"spa-type", decreasing = TRUE), ]
    tab1_dt <- tab[, .(
        Category, Country,
        `Animal/food type` = AnimalFoodType,
        `Sample type/ unit` = SampleType,
        `No. of isolates `,
        `spa-type`,
        `PVL status/ IEC genes`,
        ST, CC, `mec-gene`,
        `Inferred CC`,
        `LA, CA or HA`,
        `Inferred ST/CC & type`
    )]
    write.csv2(tab,
               file = path_csv,
               row.names = FALSE,
               quote = TRUE)
    path_csv
}
