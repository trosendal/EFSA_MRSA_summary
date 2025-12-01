## Annex E table 7: Methicillin-resistant Staphylococcus aureus in
## non-food-producing animals, clinical investigations, 2024.
source("R/functions.R")
df_prev <- read_prev()
nonfood <- c("Dogs", "Felidae", "Solipeds, domestic",
             "Land game mammals")
tab1 <- df_prev[source == "animal" &
                year == 2023 &
                SAMPCONTEXT == "Clinical investigations" &
                matrix %in% nonfood,
                .(N = sum(N), n = sum(n), prop = sum(n) / sum(N)),
                by = .(type = matrix,
                       country = country,
                       desc = matrix_txt,
                       unit = SAMPUNIT)]
tab1 <- tab1[order(type, country, desc, unit)]
tab1$result <- paste0(tab1$n, " (", round(tab1$prop * 100, 1), "%)")
tab1 <- tab1[, c(1, 2, 3, 4, 5, 8)]
names(tab1) <- c("Animal", "Country", "Production type", "Sample unit",
                 "Units tested", "Positive for MRSA (%)")
write.csv2(tab1,
           file = "tables_and_figures/MRSA_AnnexE_tableE7_new.csv",
           row.names = FALSE)
