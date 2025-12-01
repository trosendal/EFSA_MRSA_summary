# EFSA MRSA summary

## Description
This is a collection of methods to clean data from reporting counties
and produce tables and figures for the MRSA chapter of the AMR
report. Some figures are produced elsewhere but all tables should be
producible by these methods for the MRSA chapter and the MRSA annex
E.

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

```R
## Figure 1
pdf("inst/tables_and_figures/figure1.pdf",
    height = 12, width = 8,
    timestamp = FALSE)
EFSAMRSAsummary::figure1()
dev.off()

## Figure 2
pdf("tables_and_figures/figure2.pdf",
    height = 12,
    width = 8,
    timestamp = FALSE)
EFSAMRSAsummary::figure2()
dev.off()

## Table 1
EFSAMRSAsummary::table1(path_csv = "tables_and_figures/table1.csv")

## Table 2
EFSAMRSAsummary::table2(path_csv = "tables_and_figures/table2.csv")


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
