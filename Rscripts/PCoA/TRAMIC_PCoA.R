## this is an example on how PCoA plots were plotted ##

#install packages
BiocManager::install("phyloseq")

#load libraries
library("ggplot2")
library("phyloseq")
library("ape")
library("usethis")
library("devtools")
library("vegan")
library("RColorBrewer")
library(gridExtra)
library(cluster)
library(igraph)
library(tidyverse)
library(vegan)


# set working directory

## load data
#read  otu table
otu_table = read.csv("data_table_bacteria_SRS1500.csv", sep = ",", row.names = 1)
otu_table = as.matrix(otu_table)
head(otu_table)

#read in taxonomy
#separated by kingdom phylum class order family genus species
taxonomy = read.csv("taxonomy.csv", sep = ",", row.names = 1)
taxonomy = as.matrix(taxonomy)

#read in metadata
metadata = read.table("metadata.txt",row.names = 1)
head(metadata)

#read in tree (if you have one)
phy_tree = read_tree("bact_tramic_unrooted-tree.nwk")
phy_tree <- ape::multi2di(phy_tree)

#import as phyloseq objects
OTU = otu_table(otu_table, taxa_are_rows = TRUE)
TAX = tax_table(taxonomy)
META = sample_data(metadata)

## check
#check that your OTU names are consistent across objects
taxa_names(TAX)
taxa_names(OTU)
taxa_names(phy_tree)
#taxa_names(phy_tree) had ' included which I replaced by nothing
taxa_names(phy_tree) <- gsub(x = taxa_names(phy_tree), pattern = "'", replacement = "")

# make sure files have the same sample names
sample_names(OTU)
sample_names(META)
#sample_names(META) had - instead of . which I replaced
sample_names(META) <- gsub(x = sample_names(META), pattern = "-", replacement = ".")

##merge
#merge into one phyloseq object
tramic = phyloseq(OTU, TAX, META, phy_tree)
tramic

##set up
#set seed - important for replication 
set.seed(1)


## plot PCoA
# Ordinate using Principal Coordinate analysis, distance can be changed to "bray", "unifrac", "wunifrac"
tramic_pcoa <- ordinate(
  physeq = tramic, 
  method = "PCoA", 
  distance = "bray"
)

# Plot 
pl <- plot_ordination(
  physeq = tramic,
  ordination = tramic_pcoa,
  axes=c(1,2),   # this selects which axes to plot from the ordination
  color = "sampletype",
  shape = "type",
  title = "PCoA of bacterial microbial communities normalized Bray Curtis"
) + 
  scale_color_manual(values = c("yellow", "blue", "orange", "pink")) +
  geom_point(alpha = 0.7, size = 4)

# plot
pl

## statistics
# Calculate bray curtis distance matrix
tramic_unifrac_W <- phyloseq::distance(tramic, method = "bray")

# make a data frame from the scaled sample_data
sampledf <- data.frame(sample_data(tramic))

# Adonis test
adonis(tramic_bray ~ type, data = sampledf)

# test of Homegeneity of dispersion
beta <- betadisper(tramic_bray, sampledf$type)

# run a permutation test to get a statistic and a significance score
permutest(beta)