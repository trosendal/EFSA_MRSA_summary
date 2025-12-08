##' figure2
##'
##' Produce Figure 2 in the main chapter
##'
##' @param df_prev The data object
##' @param years the years to filter
##' @import data.table
##' @return A plot
##' @export
figure2 <- function(df_prev = read_prev(),
                    years =  c(2023, 2024)) {
    nonfood <- c("Dogs", "Felidae", "Solipeds, domestic")
    tab1 <- df_prev[year %in% years &
                    source == "animal" &
                    ## !(SAMPCONTEXT %in% c("Clinical investigations", "Outbreak investigation")) &
                    !(matrix %in% nonfood),
                    .(N = sum(N), n = sum(n), prop = sum(n) / sum (N)),
                    by = .(year = year,
                           type = matrix,
                           country = country,
                           desc = matrix_txt,
                           unit = SAMPUNIT)]
    tab1 <- tab1[order(type, -year, country, desc, unit)]
    tabgraph <- tab1[, .(N = sum(N),
                         n = sum(n),
                         p = sum(n)/sum(N)),
                     by = .(Food = type, country, year)]
    tabgraph <- tabgraph[N > 10, ]
    ## order of foods in graph
    foods <- tabgraph[, .(N = sum(N)), by = .(Food)][order(-N), Food]
    tabgraph[, foodrank := match(Food, foods)]
    tabgraph <- tabgraph[order(foodrank, country, -year),]
    foodnames <- c("Pigs" = "Pigs",
                   "Cattle (bovine animals)" = "Cattle",
                   "Gallus gallus (fowl)" = "Broilers",
                   "Land game mammals" = "Game animals",
                   "Small ruminants" = "Small ruminants",
                   "Turkeys" = "Turkeys")
    tabgraph$Food <- as.character(foodnames[tabgraph$Food])
    countrynames <- c("Germany" = "DE",
                      "Netherlands" = "NL",
                      "Norway" = "NO",
                      "Republic of North Macedonia" = "MK",
                      "Slovakia" = "SK",
                      "Switzerland" = "CH",
                      "Belgium" = "BE")
    tabgraph$country <- as.character(countrynames[tabgraph$country])
    mat <- matrix(c(tabgraph$N, tabgraph$n),
                  nrow = 2, byrow = TRUE)
    colnames(mat) <- paste0(tabgraph$Food,
                            " (",
                            tabgraph$country,
                            ", ",
                            tabgraph$year, ")")
    mat <- mat[, seq(ncol(mat), 1)]
    par(mar = c(8, 12, 0, 4))
    cols <- c("#409fff", "#ffb740")
    bp <- barplot(mat,
                  horiz = TRUE,
                  col = cols,
                  las = 1,
                  xlim = c(0, 3000),
                  xlab = "No. of sample units tested")
    labels <- paste0(round(mat[2,]/mat[1,] * 100, 1),
                     "% (N=", mat[1,], ")")
    text(x = colSums(mat),
         y = bp,
         labels,
         pos = 4,
         xpd = TRUE)
    legend(x = -800, y = -3,
           legend = c("No. of sample units negative for MRSA (%)",
                      "No. of sample units positive for MRSA (%)"),
           fill   = cols,
           horiz  = TRUE,
           xpd = TRUE,
           cex = 0.9)
}
