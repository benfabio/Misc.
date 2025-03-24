
### eco_dist_combine.R
### 11/06/2024 - ©Fabio Benedetti (Plant Ecology group, IPS, Uni Bern)

### ----------------------------------------------------------------------------------------------------------------

# Load necessary library
install.packages("combinat")
library("combinat")  # For generating all possible variable combinations
library("tidyverse")

### ----------------------------------------------------------------------------------------------------------------

### Creating a function to compute Euclidean distances between realized communities and their associated targets
### for all combinations and subsets of ecological variables (E1, E2, ..., En)

# CM = CommunitiesMat
# TM = TargetsMat
eco_dist_combiner <- function(CM, TM) {
  
      # Get variable names (e.g., E1, E2, E3 etc. for env variables)
      var_names <- colnames(CM) # var_names
  
      # Generate all possible combinations of variables (excluding empty set)
      all_combs <- unlist(lapply(1:length(var_names), function(x) combn(var_names, x, simplify = FALSE)), recursive = FALSE)
      # all_combs # to chekc 
  
      # Initialize list to store ecological distances in
      # distances <- list()
      
      distances <- lapply(all_combs, function(c) {
          
              # Select the relevant columns
              M_subset <- CM[,c, drop = FALSE]
              T_subset <- TM[,c, drop = FALSE]
    
              # Compute Euclidean distances for each row
              d <- apply(M_subset, 1, function(row, T_subset) {
                      dist(rbind(row, T_subset), method = "euclidean")
                  }, T_subset = T_subset
              ) # eo apply
    
              # Store results with the corresponding variable combination
              dists <- list()
              dists[[paste(c, collapse = "_")]] <- d
              
              # Return distances
              return(dists)
          
          } # eo FOO - c
      
      ) # eo lapply - all_combs
      
      # Return 'distances'
      return(distances)
  
      # Alternative: Loop through each subset of variables instead of a lapply() 
      # May be worth checking which option is fastest with system.time()
      # for(subset in all_combs) {
      #
      #     # Select the relevant columns
      #     M_subset <- CM[,subset, drop = FALSE]
      #     T_subset <- TM[,subset, drop = FALSE]
      #
      #     # Compute Euclidean distances for each row
      #     d <- apply(M_subset, 1, function(row, T_subset) {
      #             dist(rbind(row, T_subset), method = "euclidean")
      #         }, T_subset = T_subset
      #     ) # eo apply
      #
      #     # Store results with the corresponding variable combination
      #     distances[[paste(subset, collapse = "_")]] <- d
      #
      # } # eo for loop
      #
      # # Return filled 'distance_results'
      # return(distances)
  
} # eo FOO - eco_dist_combiner


# ---- Example with Dummy Data ----

# Create a small dummy dataset
set.seed(13)

# Create random data for CommunitiesMat and TargetsMat
CommunitiesMat <- matrix(runif(15, 1, 10), nrow = 10, ncol = 5)  # 3 rows (communities), 5 columns (E1-E5)
TargetsMat <- matrix(runif(15, 1, 10), nrow = 10, ncol = 5)  # 3 corresponding target rows

# Assign column names (E1 to E5)
colnames(CommunitiesMat) <- c("E1", "E2", "E3", "E4", "E5")
colnames(TargetsMat) <- c("E1", "E2", "E3", "E4", "E5")

# Check
# dim(CommunitiesMat); dim(TargetsMat)
CommunitiesMat
TargetsMat

# Run the function
results <- eco_dist_combiner(CM = CommunitiesMat, TM = TargetsMat)

# Check results ('results' is a list object)
str(results)
head(results)
results[[1]] # looks good

# Bind in table?
# t <- data.frame(dplyr::bind_rows(results))
# dim(t)
# head(t)

### ----------------------------------------------------------------------------------------------------------------