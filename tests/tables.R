## Table 1
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::table1()),
        readLines(system.file("extdata/tables_and_figures/table1.csv",
                              package = "EFSAMRSAsummary"))))

## Table 2
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::table2()),
        readLines(system.file("extdata/tables_and_figures/table2.csv",
                              package = "EFSAMRSAsummary"))))

## Table 3 (Not in actual chapter)
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::table3()),
        readLines(system.file("extdata/tables_and_figures/table3.csv",
                              package = "EFSAMRSAsummary"))))

## Table E1
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::tableE1()),
        readLines(system.file("extdata/tables_and_figures/MRSA_AnnexE_tableE1_new.csv",
                              package = "EFSAMRSAsummary"))))

## Table E2
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::tableE2()),
        readLines(system.file("extdata/tables_and_figures/MRSA_AnnexE_tableE2_new.csv",
                              package = "EFSAMRSAsummary"))))

## Table E3
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::tableE3()),
        readLines(system.file("extdata/tables_and_figures/MRSA_AnnexE_tableE3_new.csv",
                              package = "EFSAMRSAsummary"))))

## Table E4
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::tableE4()),
        readLines(system.file("extdata/tables_and_figures/MRSA_AnnexE_tableE4_new.csv",
                              package = "EFSAMRSAsummary"))))

## Table E5
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::tableE5()),
        readLines(system.file("extdata/tables_and_figures/MRSA_AnnexE_tableE5_new.csv",
                              package = "EFSAMRSAsummary"))))

## Table E6
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::tableE6()),
        readLines(system.file("extdata/tables_and_figures/MRSA_AnnexE_tableE6_new.csv",
                              package = "EFSAMRSAsummary"))))

## Table E7
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::tableE7()),
        readLines(system.file("extdata/tables_and_figures/MRSA_AnnexE_tableE7_new.csv",
                              package = "EFSAMRSAsummary"))))

## Table E8
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::tableE8()),
        readLines(system.file("extdata/tables_and_figures/MRSA_AnnexE_tableE8.csv",
                              package = "EFSAMRSAsummary"))))

## Table E9
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::tableE9()),
        readLines(system.file("extdata/tables_and_figures/MRSA_AnnexE_tableE9.csv",
                              package = "EFSAMRSAsummary"))))

## Table E10
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::tableE10()),
        readLines(system.file("extdata/tables_and_figures/MRSA_AnnexE_tableE10.csv",
                              package = "EFSAMRSAsummary"))))

## Table E12
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::tableE12()),
        readLines(system.file("extdata/tables_and_figures/MRSA_AnnexE_tableE12.csv",
                              package = "EFSAMRSAsummary"))))

## Table E13
stopifnot(
    identical(
        readLines(EFSAMRSAsummary::tableE13()),
        readLines(system.file("extdata/tables_and_figures/MRSA_AnnexE_tableE13.csv",
                              package = "EFSAMRSAsummary"))))
