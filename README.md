# Simulated Genetic Data and Pedigree Analysis

### Author: Hamdy Abdel-Shafy
### Date: August 2024
### Affiliation: Department of Animal Production, Cairo University, Faculty of Agriculture

## Overview

This project involves simulating genetic data, including pedigree, SNPs, and phenotypic data, and visualizing the results. The code generates synthetic data for individuals, their pedigree, SNP information, and phenotypic traits, then exports the data to an Excel file and creates visualizations to analyze the results.

## Dependencies

The following R packages are required to run the code:

- `dplyr`
- `tidyr`
- `writexl`
- `ggplot2`
- `igraph`

You can install these packages using the following commands:

```r
install.packages(c("dplyr", "tidyr", "writexl", "ggplot2", "igraph"))
```

## Code Description

### Simulation Parameters

- **Number of Individuals**: 500
- **Number of SNPs**: 1000
- **Number of Generations**: 5
- **Number of Herds**: 10
- **Number of Seasons**: 4
- **Number of Locations**: 5

### Data Generation

1. **Pedigree Information**: Simulates relationships between individuals, including sire and dam, and assigns gender.
2. **Environmental Factors**: Simulates herd, season, and location for each individual.
3. **SNP Data**: Generates genetic data for each individual.
4. **Phenotypic Data**: Computes body weight based on genetic effects and environmental noise.

### Functions

- **`plot_pedigree`**: Creates a pedigree graph with gender distinction. Males are represented as green squares, females as pink circles, and unknown genders as gray squares.

### Export

- **Excel File**: The simulated data is combined into an Excel file named `simulated_data.xlsx`, which includes:
  - Pedigree information
  - SNP data
  - Phenotypic data

### Visualizations

- **Phenotype Distribution**: Plots a histogram of body weight at marketing age.

## Instructions

1. **Run the Code**: Execute the provided R script in an R environment.
2. **View Results**: Check the `simulated_data.xlsx` file for the exported data. View the summary of the data and the generated plots in the R environment.

## Example Code

```r
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
```

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

