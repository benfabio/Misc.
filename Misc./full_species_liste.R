
### 18/03/2025: R script to get up to date list of accepted species names of Pteropoda and Heteropoda
### ©Fabio Benedetti, Plant Ecology group, Uni. Bern

### R Script to:
### - Load the list of species names from Pteropoda and Heteropoda with 'taxize'
### - Use the 'worrms' package to extract the AphiaID and the status associated to the name
### - Correct name if currently unaccepted in WoRMS
### - Retrieve known synonyms associated to the name
### - Rbind all species' data in table and save

### Last update: 19/03/2025

### -------------------------------------------------------------------------------------------

# Load libraries
#install.packages(("worrms","taxize"))
library("worrms")
library("taxize")
library("dplyr")

### -------------------------------------------------------------------------------------------

### Step 1: Use 'taxize' package to get the full list of species names belonging to a clade
### using WoRMS as a reference (names can be accepted or unaccepted at this stage)
### Note: adjust 'downto' argument if you're interested in even finer taxonomic resolution (subspecies etc.) 
# ?downstream
# Get all 'Pteropoda' species names in WoRMS
species_list_pteropoda <- downstream("Pteropoda", db = 'worms', downto = 'species') # takes some seconds
# Retrieve table stored inside the 'downstream' object
species_list_pteropoda2 <- species_list_pteropoda$Pteropoda
# Re-order alphabetically
species_list_pteropoda2 <- species_list_pteropoda2[order(species_list_pteropoda2$name),]
# dim(species_list_pteropoda2); head(species_list_pteropoda2)
rownames(species_list_pteropoda2) <- c(1:nrow(species_list_pteropoda2))


### Same but for 'Heteropoda'
### Note:  Heteropoda currently not accepted in WoRMS apparently - Use Pterotracheoidea
# (https://marinespecies.org/aphia.php?p=taxdetails&id=387338 )
# Note: Adjust the arguments in downstream() if necessary 
species_list_heteropoda <- downstream("Pterotracheoidea", db = 'worms', downto = 'species') 
species_list_heteropoda2 <- species_list_heteropoda$Pterotracheoidea
species_list_heteropoda2 <- species_list_heteropoda2[order(species_list_heteropoda2$name),]
# Re-order alphabetically
rownames(species_list_heteropoda2) <- c(1:nrow(species_list_heteropoda2))
# head(species_list_heteropoda2); dim(species_list_heteropoda2)


### Step 2: Write the master function to extract the species names, corretc them if needed,
### get their synonyms and living status (exctinct or not)

# For testing function below
# names <- unique(species_list_pteropoda2$name)
# sp <- names[13]; sp
sp <- "Monophora asperum"

full_species_lister <- function(sp) {
            
        tryCatch(
            
            # Code to be evaluated by tryCatch()
            expr = {
            
                message(paste("Retrieving info for ",sp, sep = ""))
        
                # Get WoRMS' AphiaID (unique numerical identifier) and status (accepted or not etc.)
                d <- data.frame(wm_records_names(name = sp)) 
                ID <- d$AphiaID
            
                # Check status, if unaccepted species name, adjust by retrieving accepted name and AphiaID
                if( d$status != "accepted" ) {
        
                    valid_name <- d$valid_name
                    valid_ID <- d$valid_AphiaID
        
                    # Get synonyms from WoRMS using wm_synonyms()
                    synonyms <- data.frame( wm_synonyms(id = valid_ID) )
        
                    # aggregate into a data.frame
                    ddf <- data.frame(
                        orig_name = sp,
                        orig_ID = ID,
                        status = d$status,
                        reason = d$unacceptreason,
                        accepted_name = valid_name,
                        accepted_ID = valid_ID,
                        isExctinct = d$isExtinct,
                        synonyms = synonyms$scientificname
                    ) # eo ddf
                
                    return(ddf)
        
                } else {
        
                    # Get synonyms from WoRMS using wm_synonyms()
                    synonyms <- data.frame( wm_synonyms(id = ID) )
        
                    # aggregate into a data.frame
                    ddf <- data.frame(
                        orig_name = sp,
                        orig_ID = ID,
                        status = d$status,
                        reason = NA,
                        accepted_name = sp,
                        accepted_ID = ID,
                        isExctinct = d$isExtinct,
                        synonyms = synonyms$scientificname
                    ) # eo ddf
                
                    return(ddf)
        
                } # eo if else loop
                
            }, error = function(e) {
                
                      # Print error message
                      message("wm_synonyms() failed likely because no known synonyms: ", e$message)
                      
                      # Get WoRMS' AphiaID (unique numerical identifier) and status (accepted or not etc.)
                      d <- data.frame(wm_records_names(name = sp)) 
                      ID <- d$AphiaID
                      
                      # Still return a ddf like the ones above but without synonyms
                      ddf <- data.frame(
                                      orig_name = sp,
                                      orig_ID = ID,
                                      status = d$status,
                                      reason = NA,
                                      accepted_name = sp,
                                      accepted_ID = ID,
                                      isExctinct = d$isExtinct,
                                      synonyms = "No synonyms found on WoRMS"
                      ) # eo ddf
                      
                      # Return this when there are no synonyms
                      return(ddf)
                       
                } # eo error function
                      
     ) # eo tryCatch
     
} # eo FUN - full_species_lister 

# To test on one sp name
#test <- full_species_lister(names[1]) 
#test

# Apply FUN in a lapply
names <- unique(species_list_pteropoda2$name)
# length(names) gives you the number of Pteropoda species names, should be 454 on the 19/03/25

# On a  simple local machine:
res <- lapply(names, full_species_lister)

# Function above can take some time to run because the clades we are interested in have many species
# If you have access to a cluster with several CPUs/GPUs, you may run the function in parallel
# computing as follows (otherwise use the simple lapply() above): 
#library("parallel")
#res <- mclapply(X = names, FUN = full_species_lister, mc.cores = 30)

# Rbind into a data.frame
table <- dplyr::bind_rows(res)
# Check
# dim(table)
# str(table)
# table[200:230,]

# How many real accepted names?
# length(unique(table$accepted_name)) # 184 accepted names in total
# length(unique(table$synonyms)) # 469 known synonyms in total
# unique(table$accepted_name) # the 184 names

# Save
write.csv(x = table, file = "table_accepted_species_synonyms_WoRMS_pteropoda_19_03_25.csv")
rm(res,names); gc()


### Re-run for Heteropoda
names <- unique(species_list_heteropoda2$name) # names
res <- lapply(names, full_species_lister)
table <- dplyr::bind_rows(res)
# dim(table) # 886 x 8
# str(table)
table[400:420,]
# How many real accepted names?
length(unique(table$accepted_name)) # 65 accepted names in total
length(unique(table$synonyms)) # 155 known synonyms in total
unique(table$accepted_name) # the 184 names

# Save
write.csv(x = table, file = "table_accepted_species_synonyms_WoRMS_heteropoda_19_03_25.csv")
rm(res,names); gc()


### -------------------------------------------------------------------------------------------
### -------------------------------------------------------------------------------------------
### -------------------------------------------------------------------------------------------