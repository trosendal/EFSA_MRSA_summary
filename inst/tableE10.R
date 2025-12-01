## Table 10: Occurrence of resistance (%) to selected antimicrobials
## in methicillin-resistant Staphylococcus aureus from food and
## animals, 2024.
source("R/functions.R")
df_AMR <- read_AMR()
subdf <- df_AMR[df_AMR$repYear == 2024, ]
tab1 <- do.call("rbind", lapply(unique(subdf$matrix_L1), function(x) {
    dfinner1 <- subdf[subdf$matrix_L1 == x, ]
    do.call("rbind", lapply(unique(dfinner1$repCountry), function(y) {
        dfinner2 <- dfinner1[dfinner1$repCountry == y, ]
        N <- length(unique(dfinner2$LABISOLCODE))
        resistance <- round((table(dfinner2$HRM_is_resistant,
                                   dfinner2$SFsubstance)/N)[2, ] * 100, 1)
        df2 <- as.data.frame(t(resistance))
        source <- x
        country <- y
        cbind(data.frame(source = source, country = country, N = N), df2)
    }))
}))
tab1 <- tab1[, c("source", "country", "N", "GEN", "KAN",
                 "STR", "CHL", "RIF", "CIP", "ERY", "CLI",
                 "Q/D", "LZD", "TIA", "MUP", "FUS", "SMX",
                 "TMP", "TET", "VAN", "CEF", "PEN")]
names(tab1)[names(tab1) == "country"] <- "Country"
write.csv2(tab1,
           file = "tables_and_figures/MRSA_AnnexE_tableE10.csv",
           row.names = FALSE)
