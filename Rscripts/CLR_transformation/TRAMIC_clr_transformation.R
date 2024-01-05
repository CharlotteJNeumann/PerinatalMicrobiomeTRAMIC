## this is an example file on how CLR transformation was performed ##

# load libraries
library(dplyr)
library(readr)
library(tibble)
library(tidyr)
library(purrr)
library(broom)
library(pheatmap)
library(plotly)
library(microbiome)
library(ggbeeswarm)
library(knitr)
library(ALDEx2)

# set working directory

#read in file
abundance_file_TRAMIC = "data_collapsed_ASV.txt"

#For the relative abundance, we take the coverage over the genome, not the raw counts.
#This implicitly normalizes for genome size.
#The coverage is calculated as the median of the coverage values calculated in 1kb blocks.
D_TRAMIC <- read_tsv(abundance_file_TRAMIC, show_col_types = FALSE) %>%
  column_to_rownames(var = "...1") %>%
  as.data.frame()

# calculate relative abundance
rel_ab_TRAMIC <- sweep(D_TRAMIC, 1, rowSums(D),`/`)

# transform counts with centered log ratio
clr_data_TRAMIC <- transform(rel_ab_TRAMIC, transform = "clr")

write.csv(clr_data_TRAMIC, file ="clr_data_TRAMIC.csv")