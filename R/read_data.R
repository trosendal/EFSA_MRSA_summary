##' prev_file
##'
##' Get the path to the packages data on prevalence
##'
##' @return A path
prev_file <- function() {
    system.file("extdata/prevalence MRSA all data.xlsx",
                package = "EFSAMRSAsummary")
}

##' isolate_file
##'
##' Get the path to the packaged data on isolates
##'
##' @return A path
isolate_file <- function() {
    system.file("extdata/MRSA AMR 2023-2024.xlsx",
                package = "EFSAMRSAsummary")
}

##' read_prev
##'
##' A function that reads the MRSA prevalence data reported by member
##' states. This is the so called "flat file" that is located here:
##'
##' "Dokument/General/SAS Files, Text Forms, National reports/
##'  3. 2024 EUSR on AMR datasets-calendar year 2025/SAS/
##'  All years/2025.07.23/prevalence MRSA all data.xlsx"
##'
##' In subsequent corrections of the data, newer versions of the file
##' could be in later dates of the same 'All years' directory. In
##' subsequent reporting years, the higher up directories will change
##' too
##'
##' @param path The local file path to the excel file
##' @param years The years to be included in the filtered data
##' @return a data.table object
##' @importFrom readxl read_xlsx
##' @export
##' @import data.table
read_prev <- function(path = prev_file(),
                      years = c("2023", "2024")) {

    ## Read in the prevalence data for MRSA from all years. The column
    ## REPYEAR is used to filter by reporting year. Each row
    ## represents a unique ST type and origin of animal or food. The
    ## count of positive in is UNITSPOSITIVE and the denomenator is in
    ## TOTUNITSTESTED and each programme is the unit combination of
    ## REPCOUNTRY and MATRIX_C. Read the data:
    df_prev <- readxl::read_xlsx(path,
                                 col_types = "text")

    ## Drop all those that are not in target years
    df_prev <- df_prev[df_prev$REPYEAR %in% years, ]

    ## Drop those that are reported below the country level
    df_prev <- df_prev[nchar(df_prev$SAMPAREA_CODE) == 2 |
                       is.na(df_prev$SAMPAREA_CODE), ]
    ## Make an id to summarize a "sampling". Apparently, you cannot
    ## report the same number of samples and positives from the same
    ## marix in the same year and country. Therefore the following
    ## rows should be collapsed:
    df_prev$samplingID <- as.numeric(
        as.factor(
            paste0(df_prev$REPYEAR,
                   df_prev$MATRIX_C,
                   df_prev$SAMPUNIT,
                   df_prev$REPCOUNTRY,
                   df_prev$SAMPAREA,
                   df_prev$TOTUNITSTESTED,
                   df_prev$TOTUNITSPOSITIVE)
        )
    )

    ## Add the datatable functionality to the object
    data.table::setDT(df_prev)

    ## Aggregate by samplingID
    df_prev <- df_prev[, {
        N <- as.numeric(TOTUNITSTESTED)[1L]
        n <- sum(as.numeric(UNITSPOSITIVE), na.rm = TRUE)
        stopifnot(all(!is.na(n)))
        prev <- n / N
        .(n = n,
          N = N,
          TOTUNITSPOSITIVE <- as.numeric(TOTUNITSPOSITIVE[1]),
          prev = prev,
          country = REPCOUNTRY[1],
          origin_country = SAMPORIG[1],
          SAMPUNIT = SAMPUNIT[1],
          SAMPCONTEXT = SAMPCONTEXT[1],
          year = REPYEAR[1],
          SPA = paste(T, collapse = ", "),
          ST = paste(ST, collapse = ", "),
          CC = paste(CC, collapse = ", "),
          source = SPECIESTYPE[1],
          matrix = MATRIX_L1[1],
          matrix_txt = MATRIX[1],
          stage = SAMPSTAGE[1])
    }
  , by = .(samplingID)]
}

##' read_AMR
##'
##' Read and cleanup the AMR isolate level data
##'
##' These are the level of clinical importance of each substance
##' according to EUvet. This is not currently used but perhaps in
##' the future we can use it for something in the summary:
##'
##' This can be used for definition of multidrug
##' resistance. Resistance to greater than 3 classes is
##' MDR. However, only the aminoglycosides are a class together so
##' basically resistance to any 3 indicates MDR. For information:
##' Steptomycin may not be the same groups as the other
##' aminogycosides since different action but is usually considered
##' so.
##'
##' spa-types t1419, t1430 and t10204 were associated to ST9 (EFSA,
##' 2009a ; Hasman et al., 2011 ; Köck et al., 2013), and classified
##' as CC1 (PubMLST1).
##'
##' STs 9 and 8325 were classified as CC1 by PubMLST1.
##' ST22 was classified as CC22 by PubMLST1.
##'
##' The data comes from the EFSA sharepoint:
##' "Dokument/General/SAS Files, Text Forms, National reports/
##'  3. 2024 EUSR on AMR datasets-calendar year 2025/SAS/
##'  Combined 2023 and 2024 data/2025.07.23/MRSA AMR 2023-2024.xlsx"
##'
##' In subsequent corrections of the data, newer versions of the file
##' could be in later dates of the same 'All years' directory. In
##' subsequent reporting years, the higher up directories will change
##' too
##'
##' @param path The local file path to the excel file
##' @param sheet The name of the sheet to read
##' @param EUvet The classification of AB into levels of importance for public health
##' @param AntibioticClass The classification of AB into functional groups
##' @param spa2CC Rules for how to classify isolates with spatype but no CC
##' @param spa2ST Rules for how to classify isoaltes with spatype but no ST
##' @param ST2CC Rules for how to classify isolates with ST but no CC
##' @param AB shortforms of AB
##' @import data.table
##' @return A data.table object
##' @export
read_AMR <- function(path = isolate_file(),
                     sheet = "QUERY_FOR_FULL_AMR_ISOL_DAT_000",
                     EUvet =
                         c("Cefoxitin" = "C", ## with the Cephalosporins
                           "Chloramphenicol" = "C",
                           "Ciprofloxacin" = "B", ## with other fluoroquinolones
                           "Clindamycin" = "C",
                           "Erythromycin" = "C",
                           "Fusidic acid" = "D",
                           "Gentamicin" = "C",
                           "Kanamycin" = "C",
                           "Linezolid" = "A",
                           "Mupirocin" = "A",
                           "Penicillin" = "D",
                           "Quinupristin/Dalfopristin" = "A", ## with the Streptogramins
                           "Rifampicin" = "A",
                           "Streptomycin" = "C",
                           "Sulfamethoxazole" = "D",
                           "Tetracycline" = "D",
                           "Tiamulin" = "C",
                           "Trimethoprim" = "D",
                           "Vancomycin" = "A"),
                     AntibioticClass =
                         c("Cefoxitin" = "Cephalosporin",
                           "Chloramphenicol" = "Amphenicol",
                           "Ciprofloxacin" = "Fluoroquinolone",
                           "Clindamycin" = "Lincosamide",
                           "Erythromycin" = "Macrolide",
                           "Fusidic acid" = "Steroid antibacterial",
                           "Gentamicin" = "Aminoglycoside",
                           "Kanamycin" = "Aminoglycoside",
                           "Linezolid" = "Oxazolidinone",
                           "Mupirocin" = "Pseudomonic acid",
                           "Penicillin" = "Natural penicillin",
                           "Quinupristin/Dalfopristin" = "Streptogramin",
                           "Rifampicin" = "Rifamycin",
                           "Streptomycin" = "Aminoglycoside",
                           "Sulfamethoxazole" = "Sulfonamide",
                           "Tetracycline" = "Tetracycline",
                           "Tiamulin" = "Pleuromutilin",
                           "Trimethoprim" = "Dihydrofolate reductase inhibitor",
                           "Vancomycin" = "Glycopeptide"),
                     spa2CC =
                         c("11" = 398, "34" = 398, "108" = 398,
                           "571" = 398, "588" = 398, "1255" = 398,
                           "1451" = 398, "1456" = 398, "1580" = 398,
                           "1793" = 398, "2011" = 398, "2330" = 398,
                           "2346" = 398, "2576" = 398, "2922" = 398,
                           "5452" = 398, "6228" = 398, "6575" = 398,
                           "10485" = 398, "19248" = 398,
                           ##(Battisti et al., 2010; EFSA 2009a ; Kinross et al.,
                           ##2017 ; Köck et al., 2013 ; Pauly et al., 2019 ;Tkadlec
                           ##et al., 2023).
                           "899" = "CC1/CC398",
                           ##If ST and/or CC was not specified, spa-type t899 was
                           ##classified as CC1/CC398 (EFSA, 2009a ; Guardabassi et
                           ##al., 2009 ; Larsen et al., 2016).
                           "127" = 1,
                           ## spa-type t127 was classified as CC1, LA-MRSA (EFSA,
                           ## 2009 ; Merialdi et al., 2019).
                           "2" = 5, "242" = 5,
                           ## spa-types t002 and t242 were classified as CC5 (Asanin
                           ## et al., 2019 ; Köck et al., 2013).
                           "8" = 8, "9" = 8,
                           ## spa-types t008 and t009 were classified as CC8 (Boost
                           ## et al., 2012 ; Cuny et al., 2016).
                           "1419" = 1, "1430" = 1, "10204" = 1,
                           ## spa-types t1419, t1430 and t10204 were associated to
                           ## ST9 (EFSA, 2009a ; Hasman et al., 2011 ; Köck et al.,
                           ## 2013), and classified as CC1 (PubMLST1).
                           "1422" = 692,
                           ## spa-type t1422 was classified as CC692 (Silva et al.,
                           ## 2020).
                           "3512" = 2343 ## spa-type t3512 was
                                         ## classified as ST2343 (Chen
                                         ## et al., 2017) and CC1
                           ## (PubMLST1).
                           ),
                     spa2ST =
                         c("1419" = 9, "1430" = 9, "10204" = 9),
                     ## spa-types t1419, t1430 and t10204 were associated to ST9 (EFSA,
                     ## 2009a ; Hasman et al., 2011 ; Köck et al., 2013), and classified
                     ## as CC1 (PubMLST1).
                     ST2CC = c("9" = 1, "8325" = 1, "22" = 22),
                     ## STs 9 and 8325 were classified as CC1 by PubMLST1.
                     ## ST22 was classified as CC22 by PubMLST1.
                     AB = c("Gentamicin" = "GEN",
                            "Kanamycin" = "KAN",
                            "Streptomycin" = "STR",
                            "Chloramphenicol" = "CHL",
                            "Rifampicin" = "RIF",
                            "Ciprofloxacin" = "CIP",
                            "Erythromycin" = "ERY",
                            "Clindamycin" = "CLI",
                            "Quinupristin/Dalfopristin" = "Q/D",
                            "Linezolid" = "LZD",
                            "Tiamulin" = "TIA",
                            "Mupirocin" = "MUP",
                            "Fusidic acid" = "FUS",
                            "Sulfamethoxazole" = "SMX",
                            "Trimethoprim" = "TMP",
                            "Tetracycline" = "TET",
                            "Vancomycin" = "VAN",
                            "Cefoxitin" = "CEF",
                            "Penicillin" = "PEN")) {

    ## Read in the AMR data from the last two years. This appears to
    ## contain the AMR testing of MRSA isolates which is les
    ## exptensive than the prevalence data. repYear is used to filter
    ## reporting year. LABISOLCODE is the unique isolate where 19
    ## analyses are done per isolate.
    df_AMR <- readxl::read_xlsx(path,
                                col_types = "text",
                                sheet = sheet)

    ## confir data.table'ness on the object
    setDT(df_AMR)

    ## Establish the substance variable
    df_AMR[, substance := {
        x <- substance_L2
        x[is.na(x)] <- substance_L1[is.na(x)]
        as.factor(x)
    }]
    stopifnot(all(!is.na(df_AMR$substance)))

    ## Classify the substances based on importance and function and
    ## give them short names:
    df_AMR[, `:=` (
        EUvet = factor(EUvet[as.character(substance)], levels = c("A", "B", "C", "D")),
        class = AntibioticClass[as.character(substance)],
        SFsubstance = AB[as.character(substance)]
    )]
    stopifnot(all(!is.na(df_AMR$EUvet)))
    stopifnot(all(!is.na(df_AMR$class)))
    stopifnot(all(!is.na(df_AMR$SFsubstance)))

    ## Infer som CCs based on ST and spa type:
    ## Update ST from spa
    df_AMR[, `:=` (ST_infer = {
        ST_infer <- ST
        ST_infer[is.na(ST_infer)] <- spa2ST[.SD[["T"]]][is.na(ST_infer)]
        ST_infer
    }
    )]

    ## Update CC for spa and ST
    df_AMR[, `:=` (CC_infer = {
        CC_infer <- CC
        CC_infer[is.na(CC_infer)] <- spa2CC[.SD[["T"]]][is.na(CC_infer)]
        CC_infer[is.na(CC_infer)] <- ST2CC[ST_infer][is.na(CC_infer)]
        CC_infer
    }
    )]

    mec <- c("mecA",
             "mecA gene = +",
             "mecA gene = +, CC = Non-398",
             "mecA gene = +, We already had the spa-type t19248 in 2019 and it was assigned to ST398 at that time.",
             "ST unknown; mecA-gene",
             "mecA-gene",
             "ST unknown, mecA-gene",
             "spa non-typeable S. aureus strain as gene for prot A is missing (double checked with EURL-AR PCR 1 and WGS). S. aureus confirmed by Maldi-Tof and WGS; mecA-gene",
             "mecA gene = +, Unfortunately, we do not yet have our own MLST data for the spa type t1849, but according to the literature, this is a \"non CC398\" (described as CC80 with elements from CC1 https://pmc.ncbi.nlm.nih.gov/articles/PMC7556507/; Ridom lists CC1).",
             "mecA gene = - ; mecC gene = +",
             "CC untypable, mecC",
             "mecC gene; Project PRR C05-i03-I-000190 - RumiRes, European Union NextGenerationEU.",
             "no presence of mec genes; Project PRR C05-i03-I-000190 - RumiRes, European Union NextGenerationEU.",
             "mecA gene; Project PRR C05-i03-I-000190 - RumiRes, European Union NextGenerationEU.")
    stopifnot(all(df_AMR$resComm %in% mec))

    ## Create a mecA column
    df_AMR[, mecA := resComm %in% mec[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 14)]]
    ## Create a mecC column
    df_AMR[, mecC := resComm %in% mec[c(10, 11, 12)]]

    ## define the isolates as food, food animals or companion animals
    nonfood <- c("Dogs", "Felidae", "Solipeds, domestic", "Land game mammals")
    foodproducing <- c("Pigs", "Cattle (bovine animals)", "Gallus gallus (fowl)",
                        "Small ruminants")
    df_AMR$animalclass <- NA
    df_AMR$animalclass[df_AMR$matrix_L1 %in% nonfood] <- "Companion animals"
    df_AMR$animalclass[df_AMR$matrix_L1 %in% foodproducing] <- "Food producing animals"
    df_AMR$animalclass[df_AMR$speciesType == "food"] <- "Food"
    stopifnot(all(!is.na(df_AMR$animalclass)))
    df_AMR[order(LABISOLCODE, substance), ]
}

##' collapse_AMR
##'
##' Summarize AMR data at the sample level Sort in order to keep the
##' fingerprints consistent
##'
##' @param df_AMR the dataobject wity AMR at the isolate level
##' @return a data.table object
##' @export
collapse_AMR <- function(df_AMR) {
    ## Ensure data is sorted
    df_AMR <- df_AMR[order(LABISOLCODE, SFsubstance), ]
    do.call("rbind", lapply(unique(df_AMR$LABISOLCODE), function(x) {
        with(df_AMR[df_AMR$LABISOLCODE == x, ], expr = {
            n <- sum(HRM_is_resistant == 1)
            innersubstance <- as.character(SFsubstance)
            fingerprint <- paste(innersubstance[as.numeric(HRM_is_resistant) == 1],
                                 collapse = "-")
            res <- as.data.frame(matrix(as.numeric(HRM_is_resistant), ncol = 19))
            names(res) <- SFsubstance
            df0 <- as.data.frame.matrix(t(table(EUvet[HRM_is_resistant == 1])))
            df1 <- data.frame(ID = x,
                              year = as.numeric(unique(repYear)),
                              country = unique(repCountry),
                              sampOrig = unique(sampOrig),
                              source = unique(speciesType),
                              matrix = unique(matrix_L1),
                              totUnitsPositive = unique(totUnitsPositive),
                              totUnitsTested = unique(totUnitsTested),
                              ST = as.numeric(unique(ST)),
                              CC = as.numeric(unique(CC)),
                              SPA = as.numeric(unique(T)),
                              fingerprint = fingerprint,
                              n = n
                              )
            cbind(df1, res, df0)
        })
    }))
}
