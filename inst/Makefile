all: \
  tables_and_figures/table1.csv \
  tables_and_figures/table2.csv \
  tables_and_figures/table3.csv \
  tables_and_figures/MRSA_AnnexE_tableE1_new.csv \
  tables_and_figures/MRSA_AnnexE_tableE2_new.csv \
  tables_and_figures/MRSA_AnnexE_tableE3_new.csv \
  tables_and_figures/MRSA_AnnexE_tableE4_new.csv \
  tables_and_figures/MRSA_AnnexE_tableE5_new.csv \
  tables_and_figures/MRSA_AnnexE_tableE6_new.csv \
  tables_and_figures/MRSA_AnnexE_tableE7_new.csv \
  tables_and_figures/MRSA_AnnexE_tableE8.csv \
  tables_and_figures/MRSA_AnnexE_tableE9.csv \
  tables_and_figures/MRSA_AnnexE_tableE10.csv \
  tables_and_figures/MRSA_AnnexE_tableE12.csv \
  tables_and_figures/MRSA_AnnexE_tableE13.csv \
  tables_and_figures/figure1.pdf \
  tables_and_figures/figure2.pdf \

.PHONY: convert_to_png
convert_to_png: \
  tables_and_figures/figure1.png \
  tables_and_figures/figure2.png

.PHONY: clean
clean:
	rm tables_and_figures/*.pdf
	rm tables_and_figures/*.png

COMMON_DEPS = \
  inst/extdata/MRSA\ AMR\ 2023-2024.xlsx \
  inst/extdata/prevalence\ MRSA\ all\ data.xlsx \
  R/functions.R

## Main chapter tables
tables_and_figures/table1.csv: R/table1.R $(COMMON_DEPS)
	Rscript R/table1.R

tables_and_figures/table2.csv: R/table2.R $(COMMON_DEPS)
	Rscript R/table2.R

## Annex tables
tables_and_figures/MRSA_AnnexE_tableE1_new.csv: R/tableE1.R $(COMMON_DEPS)
	Rscript R/tableE1.R

tables_and_figures/MRSA_AnnexE_tableE2_new.csv: R/tableE2.R $(COMMON_DEPS)
	Rscript R/tableE2.R

tables_and_figures/MRSA_AnnexE_tableE3_new.csv: R/tableE3.R $(COMMON_DEPS)
	Rscript R/tableE3.R

tables_and_figures/MRSA_AnnexE_tableE4_new.csv: R/tableE4.R $(COMMON_DEPS)
	Rscript R/tableE4.R

tables_and_figures/MRSA_AnnexE_tableE5_new.csv: R/tableE5.R $(COMMON_DEPS)
	Rscript R/tableE5.R

tables_and_figures/MRSA_AnnexE_tableE6_new.csv: R/tableE6.R $(COMMON_DEPS)
	Rscript R/tableE6.R

tables_and_figures/MRSA_AnnexE_tableE7_new.csv: R/tableE7.R $(COMMON_DEPS)
	Rscript R/tableE7.R

tables_and_figures/MRSA_AnnexE_tableE8.csv: R/tableE8.R $(COMMON_DEPS)
	Rscript R/tableE8.R

tables_and_figures/MRSA_AnnexE_tableE9.csv: R/tableE9.R $(COMMON_DEPS)
	Rscript R/tableE9.R

tables_and_figures/MRSA_AnnexE_tableE10.csv: R/tableE10.R $(COMMON_DEPS)
	Rscript R/tableE10.R

tables_and_figures/MRSA_AnnexE_tableE12.csv: R/tableE12.R $(COMMON_DEPS)
	Rscript R/tableE12.R

tables_and_figures/MRSA_AnnexE_tableE13.csv: R/tableE13.R $(COMMON_DEPS)
	Rscript R/tableE13.R

## Figures:
tables_and_figures/figure1.pdf: R/figure1.R $(COMMON_DEPS)
	Rscript R/figure1.R

tables_and_figures/figure1.png: tables_and_figures/figure1.pdf
	magick -density 300 tables_and_figures/figure1.pdf -quality 100 \
	-background white -alpha remove \
	-strip -define png:exclude-chunk=time tables_and_figures/figure1.png

tables_and_figures/figure2.pdf: R/figure2.R $(COMMON_DEPS)
	Rscript R/figure2.R

tables_and_figures/figure2.png: tables_and_figures/figure2.pdf
	magick -density 300 tables_and_figures/figure2.pdf -quality 100 \
	-background white -alpha remove \
	-strip -define png:exclude-chunk=time tables_and_figures/figure2.png

## An extra table
tables_and_figures/table3.pdf: R/table3.R $(COMMON_DEPS)
	Rscript R/table3.R
