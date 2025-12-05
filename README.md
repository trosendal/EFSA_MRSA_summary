# EFSA MRSA summary

## Description
This is a collection of methods to clean data from reporting counties
and produce tables and figures for the MRSA chapter of the AMR
report. Some figures are produced elsewhere but all tables should be
producible by these methods for the MRSA chapter and the MRSA annex E.

The tables are produced in csv format and subsequently need to be
tidied up a little in word to make the final result. This is achieved
by opening the csv file in Microsoft excel or libreoffice calc and
then copying to Microsoft word or to libreoffice writer to make a
table. The table formatting is created there including merging of
cells and hence removal of duplicated data. This file is saved as
.docx format and uploaded to EFSA's sharepoint. It is not recommended
to copy a table object from a desktop libreoffice writer session to a
web-based word session in EFSA's sharepoint as this causes formatting
problems. However, a copy paste operation from an uploaded file to in
the web-based Microsoft word to another file works fine, at least when
track changes to turned off. If track changes are enabled then copy
paste of large tables gets very messy.

## Usage
To use this package you need to install it first in R. A convenient
way to do that is like this:
```R
remotes::install_github("trosendal/EFSAMRSAsummary", upgrade = "never")
```

Once installed you can build all the tables and figures like
this. They will be saved in a directory where you are running R from
named: "output":

```R
if (!dir.exists("output")) {
    dir.create("output")
}
## Figure 1
pdf("output/figure1.pdf",
    height = 12, width = 8,
    timestamp = FALSE)
EFSAMRSAsummary::figure1()
dev.off()

## Figure 2
pdf("output/figure2.pdf",
    height = 12,
    width = 8,
    timestamp = FALSE)
EFSAMRSAsummary::figure2()
dev.off()

## Table 1
EFSAMRSAsummary::table1(path_csv = "output/table1.csv")

## Table 2
EFSAMRSAsummary::table2(path_csv = "output/table2.csv")

## Table 3 (Not in actual chapter)
EFSAMRSAsummary::table3(path_csv = "output/table3.csv")

## Table E1
EFSAMRSAsummary::tableE1(path_csv = "output/MRSA_AnnexE_tableE1_new.csv")

## Table E2
EFSAMRSAsummary::tableE2(path_csv = "output/MRSA_AnnexE_tableE2_new.csv")

## Table E3
EFSAMRSAsummary::tableE3(path_csv = "output/MRSA_AnnexE_tableE3_new.csv")

## Table E4
EFSAMRSAsummary::tableE4(path_csv = "output/MRSA_AnnexE_tableE4_new.csv")

## Table E5
EFSAMRSAsummary::tableE5(path_csv = "output/MRSA_AnnexE_tableE5_new.csv")

## Table E6
EFSAMRSAsummary::tableE6(path_csv = "output/MRSA_AnnexE_tableE6_new.csv")

## Table E7
EFSAMRSAsummary::tableE7(path_csv = "output/MRSA_AnnexE_tableE7_new.csv")

## Table E8
EFSAMRSAsummary::tableE8(path_csv = "output/MRSA_AnnexE_tableE8.csv")

## Table E9
EFSAMRSAsummary::tableE9(path_csv = "output/MRSA_AnnexE_tableE9.csv")

## Table E10
EFSAMRSAsummary::tableE10(path_csv = "output/MRSA_AnnexE_tableE10.csv")

## Table E12
EFSAMRSAsummary::tableE12(path_csv = "output/MRSA_AnnexE_tableE12.csv")

## Table E13
EFSAMRSAsummary::tableE13(path_csv = "output/MRSA_AnnexE_tableE13.csv")
```

## Future development
### Write word tables directly
It would be better to write the  Microsoft word tables directly from
R. The xml structure is well documented and this should therefore be
possible and make the whole process more reproducible.
### Publish sample data
The data is not included today since it is not clear if it is ok to
publish the unreported data publicly. Perhaps this can be done with a
clear disclaimer that it is a draft and not the final version
