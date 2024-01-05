install.packages("ggplot2")
library("ggplot2")
install.packages("ggpubr")
library("ggpubr")
install.packages("tidyverse")
library("tidyverse")

# Set the working directory containing the files to import the data from and save plots into.
setwd("C:/Users/charl/Nextcloud/2021_TRAMIC_Manuscript/NGS-post-normalization/heatmap")

# check that only samples of which both informations are available, are included in the tables
# therefore I deleted P14-H2, P16H2 from the data-table and P17-H1 from the metabolomic table
# P14-H1s was renamed in P13-H1


####prepare the input

### Metabolomic data.

## Import the data
metabolomic_data <- read_csv("metabolomics_sign_mpre.csv")
metabolomic_data <- column_to_rownames(metabolomic_data, var = "Sample_ID")

## Select the top N metabolites in the dataframe.
# Order the data frame by the abundance of the genera.
metabolite_table_ordered <- metabolomic_data[,order(colSums(metabolomic_data),decreasing=TRUE)]
# Extract a list of top N metabolite names.
N <- 50
metabolite_list <- colnames(metabolite_table_ordered)[1:N]
# Select only the top N most abundant genus data.
metabolite_table_top <- data.frame(metabolite_table_ordered[,colnames(metabolite_table_ordered) %in% metabolite_list])
names(metabolite_table_top)


### Genus microbiota data.

## Import the genus data
genus_data <- read_csv("data_table_bacteria_urine_mpre.csv")
# Make the "Sample_ID" column into rownames.
genus_data <- column_to_rownames(genus_data, var = "Sample_ID")
# Convert the count data into percentages.
genus_data_prop <- genus_data/rowSums(genus_data)*100
# Make the new rownames into a column named "Sample".
genus_data_prop <- rownames_to_column(as.data.frame(genus_data_prop), "Sample_ID")


## Select the top N genera in the dataframe.
# Order the data frame by the abundance of the genera.
genus_table_ordered <- genus_data[,order(colSums(genus_data),decreasing=TRUE)]
# Extract a list of top N Genera names.
N <- 20
taxa_list <- colnames(genus_table_ordered)[1:N]
# Select only the top ten most abundant genus data.
genus_table_top <- data.frame(genus_table_ordered[,colnames(genus_table_ordered) %in% taxa_list])
names(genus_table_top)



# Merge the metabolomic data and the genus data together using the "Sample_ID" column to select only
# genus data for samples that have matching metabolomic data.
# read files in again, so that Sample_ID is not row names
genus_data <- read_csv("data_table_bacteria_urine_mpre.csv")
metabolomic_data <- read_csv("metabolomics_sign_mpre.csv")
merged_all_data <- merge(metabolomic_data, genus_data, by = c("Sample_ID"))


#### Correlation analysis

# Put the two data frames into x and y files to put into the correlation script.
x <- genus_table_top
y <- metabolite_table_top

# Create single grouping variable.
# If not using a grouping variable just leave this set to a created variable of group 1.
study_no_sub <- dplyr::select(merged_all_data,
                              c(Sample_ID))
study_no_sub$group <- study_no_sub$Sample_ID
study_no_sub$group <- 1
# Add grouping data even if group is not used.
groups <- study_no_sub[,2]

#You can use kendall, spearman, or pearson below:
method <- "pearson"
colnames(x)
colnames(y)

#Now calculate the correlation between individual Taxa and the environmental data
df<-NULL
for(i in colnames(x)){
  for(j in colnames(y)){
    for(k in unique(groups)){
      a<-x[groups==k,i,drop=F]
      b<-y[groups==k,j,drop=F]
      tmp<-c(i,j,cor(a[complete.cases(b),],b[complete.cases(b),],use="everything",method=method),cor.test(a[complete.cases(b),],b[complete.cases(b),],method=method)$p.value,k)
      if(is.null(df)){
        df<-tmp
      }
      else{
        df<-rbind(df,tmp)
      }
    }
  }
}

a
b
i

df <- data.frame(row.names=NULL,df)
df

colnames(df)<-c("Genus", "Metabolite", "Correlation", "Pvalue", "Group")

df$Pvalue<-as.numeric(as.character(df$Pvalue))

df$AdjPvalue<-rep(0,dim(df)[1])

df$Correlation<-as.numeric(as.character(df$Correlation))

#You can adjust the p-values for multiple comparison using Benjamini & Hochberg (1995):
# 1 -> donot adjust
# 2 -> adjust Env + Type (column on the correlation plot)
# 3 -> adjust Taxa + Type (row on the correlation plot for each type)
# 4 -> adjust Taxa (row on the correlation plot)
# 5 -> adjust Env (panel on the correlation plot)
adjustment_label <- c("NoAdj","AdjEnvAndType","AdjTaxaAndType","AdjTaxa","AdjEnv")
adjustment <- 1

if(adjustment==1){
  df$AdjPvalue<-df$Pvalue
} else if (adjustment==2){
  for(i in unique(df$Metabolite)){
    for(j in unique(df$Type)){
      sel<-df$Metabolite==i & df$Type==j
      df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
    }
  }
} else if (adjustment==3){
  for(i in unique(df$Taxa)){
    for(j in unique(df$Type)){
      sel<-df$Taxa==i & df$Type==j
      df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
    }
  }
} else if (adjustment==4){
  for(i in unique(df$Taxa)){
    sel<-df$Taxa==i
    df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
  }
} else if (adjustment==5){
  for(i in unique(df$Metabolite)){
    sel<-df$Metabolite==i
    df$AdjPvalue[sel]<-p.adjust(df$Pvalue[sel],method="BH")
  }
}

#Now we generate the labels for significant values
df$Significance <- cut(df$AdjPvalue, breaks=c(-Inf, 0.001, 0.01, 0.05, Inf), label=c("***", "**", "*", ""))
df


ggplot(aes(x = Metabolite,
           y = Genus,
           fill = Correlation),
       data = df) +
  geom_tile() +
  scale_fill_gradient2(low = "slateblue",
                       mid = "white",
                       high = "red") +
  labs(title="Pearson",
       x="Genus",
       y = "Metabolites") +
  theme(legend.position = "right",
        plot.title = element_text(size=8),
        legend.title = element_text(size=8), #Define the legend title.
        legend.text = element_text(size=8), #Define the size of the legend text.
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2), #Define the plot margin size.
        panel.spacing = unit(.25, "lines"),
        legend.background = element_blank(), #Remove the grey background.
        panel.background = element_blank(), #Remove the grey plot background.
        panel.border = element_blank(), #Remove the plot border.
        panel.grid.major = element_blank(), #Remove the major plot gridlines.
        panel.grid.minor = element_blank(), #Remove the minor plot gridlines.
        strip.text.x = element_text(size = 8, colour = "black", angle = 90, vjust = .5, hjust = 0.5),
        axis.title.x = element_text(size=8, colour = "black"), #Define x axis title.
        axis.text.x = element_blank(), #Define x axis labels.
        axis.title.y = element_text(size=8, colour = "black"), #Define y axis title text size.
        axis.text.y = element_text(size=8, colour = "black"), #Define the axis label text size.
        axis.ticks.x = element_blank()
  ) +
  geom_text(aes(label=Significance),
            color="black",
            size=3)+
  labs(y=NULL, x=NULL, fill=method) +
  facet_grid(. ~ Metabolite,
             drop=TRUE,scale="free",
             space="free_x"
             #labeller=labeller(Env_labeller)
  )

ggsave("pearson correlation_mpre.png",
       width = 20,
       height = 25,
       units = c("cm"),
       dpi = 300)

write.csv(df,"pearson_correlation_mpre.csv")
