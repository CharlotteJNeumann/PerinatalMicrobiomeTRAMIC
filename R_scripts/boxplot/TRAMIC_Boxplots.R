# this script is an example on one dataset on how differentially abundance boxplots were plotted

#load libraries
library(ggplot2)
library(dplyr)
library(reshape)

# set working directory

## read, melt and adjust input file
# read input file
  # make sure that taxa have numbers before the name for designated order
  data <-read.table("all_V.csv", header=TRUE, sep=",")
# melt data
  melted_proportion <- melt(data)
# wirte as csv
  write.csv(melted_proportion, file ="melted_proportion.csv")

  #adjust melted file: delete numbers in "variable", change labels in "id", "taxon", "type", "value" 
# read melted input file
data <- read.csv("melted_proportion_all_V.csv", header=TRUE, sep=",", row.names="id")

#define group colors
group.colors <- c(np = "red", mpre = "forestgreen", mpost ="blue")

# settings
genus_H <- ggplot(data, aes(x = taxon, y = value, fill = type)) +
  geom_boxplot() + 
  labs(fill = "group") + 
  geom_point(position=position_jitterdodge(),alpha=0.3, size=1) +
  theme_bw(base_size = 16)+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
  scale_fill_manual(values=group.colors)

# plot
genus_H +labs(x="", y="normalized clr values", title="differentially abundance vaginal")
