
# Simulated Genetic Data and Pedigree Analysis

### Author: Hamdy Abdel-Shafy
### Date: August 2024
### Affiliation: Department of Animal Production, Cairo University, Faculty of Agriculture



# The following R packages are required to run the code, if not already installed, you can use the following commands:

install.packages(c("dplyr", "tidyr", "writexl", "ggplot2", "igraph"))


# Load necessary libraries
library(dplyr)
library(tidyr)
library(writexl)
library(ggplot2)
library(igraph)

# Set seed for reproducibility
set.seed(123)

# Parameters
n_individuals <- 500
n_snps <- 1000
n_generations <- 5
n_herds <- 10
n_seasons <- 4
n_locations <- 5

# Simulate pedigree information
pedigree <- data.frame(
  Individual = 1:n_individuals,
  Sire = sample(c(0, 1:n_individuals), n_individuals, replace = TRUE),
  Dam = sample(c(0, 1:n_individuals), n_individuals, replace = TRUE),
  Generation = rep(1:n_generations, length.out = n_individuals),
  Gender = sample(c("Male", "Female"), n_individuals, replace = TRUE)
)

# Ensure no self-parenting
pedigree <- pedigree %>%
  mutate(Sire = ifelse(Sire == Individual, 0, Sire),
         Dam = ifelse(Dam == Individual, 0, Dam))

# Simulate environmental factors
env_factors <- data.frame(
  Individual = 1:n_individuals,
  Herd = sample(1:n_herds, n_individuals, replace = TRUE),
  Season = sample(1:n_seasons, n_individuals, replace = TRUE),
  Location = sample(1:n_locations, n_individuals, replace = TRUE)
)

# Simulate SNP data
snp_data <- matrix(sample(c(0, 1, 2), n_individuals * n_snps, replace = TRUE), 
                    nrow = n_individuals, ncol = n_snps)
colnames(snp_data) <- paste0("SNP", 1:n_snps)
rownames(snp_data) <- paste0("Ind", 1:n_individuals)

# Add Individual ID column
snp_data_df <- as.data.frame(snp_data)
snp_data_df$Individual <- rownames(snp_data_df)
snp_data_df <- snp_data_df %>%
  select(Individual, everything())

# Simulate phenotypic data
heritability <- 0.35
nongenetic_variance <- 1 - heritability

# Genetic effect (proportional to SNP data)
genetic_effect <- rowMeans(snp_data) * heritability + rnorm(n_individuals, 0, sqrt(nongenetic_variance))
environmental_effect <- rnorm(n_individuals, 0, 1)  # Random noise
phenotypic_data <- genetic_effect + environmental_effect

# Create data frame for phenotypic data with environmental factors
phenotypic_data_df <- data.frame(
  Individual = paste0("Ind", 1:n_individuals),  # Ensure matching type with SNP data
  BodyWeight = phenotypic_data
)

# Convert 'Individual' in env_factors to character
env_factors$Individual <- paste0("Ind", env_factors$Individual)

# Combine phenotypic data with environmental factors
phenotypic_data_df <- phenotypic_data_df %>%
  left_join(env_factors, by = "Individual")

# Combine all data into a list for export
simulated_data <- list(
  Pedigree = pedigree,
  SNPs = snp_data_df,
  Phenotypes = phenotypic_data_df
)

# Export to Excel
write_xlsx(simulated_data, path = "simulated_data.xlsx")

# Display summary
print(summary(pedigree))
print(head(snp_data_df))
print(head(phenotypic_data_df))

# Plot phenotype distribution
ggplot(phenotypic_data_df, aes(x = BodyWeight)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Body Weight at Marketing Age",
       x = "Body Weight",
       y = "Frequency") +
  theme_minimal()

# Function to create pedigree graph with gender distinction
plot_pedigree <- function(pedigree_data) {
  # Create edges for the graph
  edges <- data.frame(
    from = c(pedigree_data$Individual, pedigree_data$Sire, pedigree_data$Individual, pedigree_data$Dam),
    to = c(pedigree_data$Sire, pedigree_data$Individual, pedigree_data$Dam, pedigree_data$Individual)
  )
  
  # Create vertices for the graph
  vertices <- data.frame(name = unique(c(edges$from, edges$to)))
  
  # Add gender information to vertices
  vertices <- vertices %>%
    left_join(pedigree_data %>% select(Individual, Gender), by = c("name" = "Individual"))
  
  # Fill in any missing gender information
  vertices$Gender[is.na(vertices$Gender)] <- "Unknown"
  
  # Create the graph object
  g <- graph_from_data_frame(d = edges, vertices = vertices, directed = TRUE)
  
  # Define color and shape based on gender
  V(g)$color <- ifelse(V(g)$Gender == "Male", "green", 
                       ifelse(V(g)$Gender == "Female", "pink", "gray"))
  V(g)$shape <- ifelse(V(g)$Gender == "Male", "square", 
                       ifelse(V(g)$Gender == "Female", "circle", "square"))
  
  # Plot the graph
  plot(g, 
       main = "Pedigree Tree with Gender Distinction",
       vertex.label = V(g)$name,
       vertex.size = 10,
       vertex.label.cex = 0.6,
       edge.arrow.size = 0.3,
       edge.color = "gray",
       vertex.color = V(g)$color,
       vertex.shape = V(g)$shape,
       layout = layout_as_tree(g))
}


# Plot pedigree for all individuals
plot_pedigree(pedigree)

