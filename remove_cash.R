# Remove Cash r cran

# remove all environment
rm(list = ls())

# Restart RStudio
.rs.restartR()


# gives the amount of memory obtained by the OS
memory.size(max=T) 

# gives the amount of memory being used
memory.size(max=F) 


#will clear all objects includes hidden objects.
rm(list = ls(all.names = TRUE))
gc()


# Clean console

cat("\014")  
