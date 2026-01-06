##' ST2CC
##'
##' STs 9 and 8325 were classified as CC1 by PubMLST1.
##' ST22 was classified as CC22 by PubMLST1.
##'
##' @param ST a vector of STs
##' @return A vector of inferred CC from ST
ST2CC <- function(ST) {
    ST2CC <- c("9" = 1,
               "8325" = 1,
               "22" = 22)
    ST2CC[ST]
}

##' spa2CC
##'
##' @param spa a vector of spa's
##' @return A vector of inferred CC from spa
spa2CC <- function(spa) {
    spa2CC <- c("11" = 398, "34" = 398, "108" = 398,
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
                )
    spa2CC[spa]
}

##' spa2ST
##'
##' @param spa a vector of spa's
##' @return A vector of inferred ST from spa
spa2ST <- function(spa) {
    spa2ST <-
        c("1419" = 9, "1430" = 9, "10204" = 9)
    ## spa-types t1419, t1430 and t10204 were associated to ST9 (EFSA,
    ## 2009a ; Hasman et al., 2011 ; Köck et al., 2013), and
    ## classified as CC1 (PubMLST1).
    spa2ST[spa]
}

##' AB
##'
##' Translate antibiotic names to shortform
##'
##' @param ab A vector of longform antibiotic names
##' @return A vector of shortform names for antibiotics
AB <- function(ab) {
    AB <- c("Gentamicin" = "GEN",
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
            "Penicillin" = "PEN")
    stopifnot(all(ab %in% names(AB)))
    AB[ab]
}

##' EUvet
##'
##' Classify AB into importance according to EUvet
##'
##' @param ab A vector of longform antibiotic names
##' @return The class in A B C D where A is the most important
EUvet <- function(ab) {
    EUvet <-
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
          "Vancomycin" = "A")
    stopifnot(all(ab %in% names(EUvet)))
    EUvet[ab]
}

##' AntibioticClass
##'
##' Classify AB into functional groups
##'
##' @param ab A vector of longform antibiotic names
##' @return The class of antibiotic
AntibioticClass <- function(ab) {
    AntibioticClass <- c("Cefoxitin" = "Cephalosporin",
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
                         "Vancomycin" = "Glycopeptide")
    stopifnot(all(ab %in% names(AntibioticClass)))
    AntibioticClass[ab]
}

##' host_association
##'
##' Estimate the host association of the SPA type
##'
##' @param spa The spa type
##' @param explain Spit out an explanation of the classification scheme
##' @export
##' @return A vector of the host association
host_association <- function(spa, explain = FALSE) {
    tl <- c("2" = "HA",
            "8" = "CA/HA",
            "9" = "CA/HA",
            "11" = "LA",
            "34" = "LA",
            "108" = "LA",
            "127" = "LA",
            "235" = "LA",
            "242" = "HA",
            "538" = "LA",
            "571" = "LA",
            "588" = "LA",
            "693" = "LA",
            "779" = "LA",
            "899" = "LA",
            "1255" = "LA",
            "1419" = "LA",
            "1422" = "LA",
            "1430" = "LA",
            "1451" = "LA",
            "1456" = "LA",
            "1457" = "LA",
            "1580" = "LA",
            "1793" = "LA",
            "2011" = "LA",
            "2330" = "LA",
            "2346" = "LA",
            "2576" = "LA",
            "2922" = "LA",
            "3075" = "LA",
            "3423" = "LA",
            "3512" = "LA",
            "4652" = "LA",
            "5104" = "LA",
            "5452" = "LA",
            "5524" = "LA",
            "6228" = "LA",
            "6575" = "LA",
            "6608" = "LA",
            "10204" = "LA",
            "10485" = "LA",
            "14089" = "LA",
            "15528" = "LA",
            "16666" = "LA",
            "19248" = "LA",
            "20072" = "HA",
            "21217" = "LA")
    if (isFALSE(explain)) return(tl[spa])
    levels <- unique(tl)
    cat("\n\n", paste0(sapply(levels, function(x) {
        paste0("The following SPA types were classified as: ", x, " : ", paste(names(tl[tl == x]), collapse = ", "))
    }), collapse = "\n"), "\n\n", sep = "")
}
