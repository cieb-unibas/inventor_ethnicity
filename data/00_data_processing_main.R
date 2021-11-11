#######################################################################
# Description:    This script loads inventor data from the USPTO and  #
#                 the EPO to investigate the ethnicity of inventors   #
#                 across countries and over time.                     #
# Authors:        Matthias Niggli/CIEB University of Basel            #
# Last Revised:   11.11.2021                                          #
#######################################################################

##################################################
## Set directories, load packages and functions ##
##################################################

# directories  -----------------------------------------------------------------
DatDir <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_ethnicity"){
  print("Working directory corresponds to repository directory")}else{
    print("Make sure your working directory is the repository directory.")}

# Load inventors --------------------------------------------
inv_dat <- readRDS(paste0(DatDir, "/created data/inventor_origin.rds"))
techfield_grouping <- read.table("data/techfield_grouping.txt", 
                                  header = TRUE, sep = ";")
inv_dat <- merge(inv_dat, techfield_grouping, by = "tech_field", all.x = TRUE)
print("Data on patent inventors loaded")

# Load functions --------------------------------------------
source("data/01_data_processing_helper_functions.R")

#############################################################################
######## Figure 2: Non-Western Origin Shares in Western countries ###########
#############################################################################

# specify parameters
COUNTRIES <- c("US", "CA", "GB", "FR", "DE", "IT", "ES", "CH", "NL", "SE", "DK", "AT", "BE")
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")

# create the data
plot2_df <- foreign_shares_fun(COUNTRIES, NON_WESTERN_ORIGIN, START_YEAR = 1985, END_YEAR = 2015)

# test/sanity check:
if(length(unique(plot2_df$country)) == length(COUNTRIES)){
  print("Data for Figure 2 successfully created")}else{
    warning("Number of countries in the created data does not correspond to function input.")}

#####################################################################################################
######## Figure 3: Non-Western Origin Shares in Technological Fields of Western countries ###########
#####################################################################################################

# specify parameters
COUNTRIES <- c("US", "CA", "GB", "FR", "DE", "IT", "ES", "CH", "NL", "SE", "DK", "AT", "BE")
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")
TECHFIELDS <- as.character(techfield_grouping$tech_field)

# create the data
plot3_df <- non_western_techfield(countries = COUNTRIES, 
                                 origins = NON_WESTERN_ORIGIN,
                                 techfields = TECHFIELDS,
                                 start_year = 1980, end_year = 2015,
                                 min_inventors = 30, MA_5 = TRUE)
# add techfield names
plot3_df <- merge(plot_df, techfield_grouping[, c("tech_field", "tech_field_name")], 
                 by = "tech_field", all.x = TRUE)

# test/sanity check
if(length(unique(plot3_df$country)) == length(COUNTRIES)){
  print("Data for Figure 3 successfully created")}else{
    warning("Number of countries in the created data does not correspond to function input.")}

##################################
######## Save the datasets #######
##################################

for (i in seq(2,3)) {
  dat_name <- paste0("plot", i, "_df")
  write.csv(as.name(dat_name), 
            paste0("report/", dat_name, ".csv"),
            row.names = FALSE)
  paste("Data for plot", i, "saved.")
  }

