install.packages("dplyr")
install.packages("ggplot2")
install.packages("sf")
install.packages("plotly", repos= "http://cran.rstudio.com/", dependencies=TRUE)
install.packages("tidyr")

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

if (!requireNamespace("VennDiagram", quietly = TRUE)) {
  install.packages("VennDiagram")
}
library(VennDiagram)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("GeneOverlap")

library(GeneOverlap)




#Read in the data 
data <- read_csv("/Users/ashleyvictor/Downloads/data_file.csv")

long_gene_data <- data %>%
  pivot_longer(cols = c(Mice_Liver_Genes, Mice_Blood_Genes), names_to = "Tissue", values_to = "Gene") %>%
  filter(!is.na(Gene)) .

# Assuming you just want to visualize the count of unique genes in each tissue type
gene_counts <- long_gene_data %>%
  group_by(Tissue) %>%
  summarise(Count = n_distinct(Gene))

# Plot
ggplot(gene_counts, aes(x = Tissue, y = Count, fill = Tissue)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Count of Unique Genes in Liver and Blood", x = "Tissue Type", y = "Count of Unique Genes")

# Assuming data$Mice_Liver_Genes and data$Mice_Blood_Genes are your gene lists
liver_genes <- na.omit(data$Mice_Liver_Genes)
blood_genes <- na.omit(data$Mice_Blood_Genes)


# Plot Venn diagram
venn.plot <- venn.diagram(
  x = list(Liver = liver_genes, Blood = blood_genes),
  filename = NULL,
  category.names = c("Liver", "Blood"),
  output = TRUE,
  height = 800,
  width = 800,
  resolution = 300,
  compression = "none",
  lwd = 2,
  col = "transparent",
  fill = c("red", "blue"),
  alpha = 0.5,
  cex = 1.5,
  fontfamily = "sans",
  fontface = "bold",
  cat.col = c("darkred", "darkblue"),
  cat.cex = 1.5,
  cat.fontfamily = "sans",
  cat.fontface = "bold",
  cat.dist = 0.025,
  cat.pos = c(-20, 14)
)

# Display the plot
grid.draw(venn.plot)

overl <- newGeneOverlap(
  unique(liver_genes),
  unique(blood_genes),
  genome.size = NULL,
  spec = c("mm9.gene")
  
)

overl <- testGeneOverlap(overl)
print(overl)
