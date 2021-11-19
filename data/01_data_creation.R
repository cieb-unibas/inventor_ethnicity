#######################################################################
# Description:    This script loads inventor data from the USPTO and  #
#                 the EPO to investigate the ethnicity of inventors   #
#                 across countries and over time.                     #
# Authors:        Matthias Niggli/CIEB University of Basel            #
# Last Revised:   19.11.2021                                          #
#######################################################################

##################################################
## Set directories, load packages and functions ##
##################################################

#### directories  -----------------------------------------------------------------
DatDir <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_ethnicity"){
  print("Working directory corresponds to repository directory")}else{
    print("Make sure your working directory is the repository directory.")}
# setwd(paste0(getwd(), "/Innoscape-GitHub-Repos/inventor_ethnicity"))

#### Load libraries --------------------------------------------
library("tidyverse")
library("countrycode")

#### Load inventors --------------------------------------------
# contains 3'402'686 inventors with predicted information regarding their ethnic origin
inv_dat <- readRDS(paste0(DatDir, "/created data/inventor_origin.rds"))
print("Data on patent inventors loaded")

#### Load technological group information -------------------------------------
# contains a refined classification of technological groups.
# This selection is motivated from Schmoch (2008):
# http://www.world-intellectual-property-organization.com/edocs/mdocs/classifications/en/ipc_ce_41/ipc_ce_41_5-annex1.pdf
# and Hall (2001):
# https://www.nber.org/system/files/working_papers/w8498/w8498.pdf
techfield_grouping <- read.table(
  paste0(DatDir, "/created data/tech_group_crosswalk/techfield_grouping.txt"), 
  header = TRUE, sep = ";"
  )
print("Data on technological groups loaded")

#### merge information together ---------------------------------
inv_dat <- merge(inv_dat, techfield_grouping, by = "tech_field", all.x = TRUE)

#### Load helper functions --------------------------------------------
source("data/02_data_creation_funs.R")

#############################################################################
######## Figure 2: Non-Western Origin Shares in Western countries ###########
#############################################################################

#### specify parameters
COUNTRIES <- c("US", "CA", 
               "CH",
               "GB", "FR", "DE", "IT", 
               "ES", "NL", "SE", "DK", "AT"#, "BE"
               )
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")

#### create the data for plotting
plot2_df <- foreign_shares_fun(COUNTRIES, 
                               NON_WESTERN_ORIGIN, 
                               START_YEAR = 1985, END_YEAR = 2015)

#### test/sanity check:
if(length(unique(plot2_df$country)) == length(COUNTRIES)){
  print("Data for Figure 2 successfully created")}else{
    warning("Number of countries in the created data does not correspond to function input.")}

#### visualize the data
ggplot(plot2_df, aes(x = p_year, y = share, color = origin))+
  facet_wrap(.~ country)+
  geom_line()+
  labs(x = "Year", y = "Shares of Non-Western Ethnic Origins", color = "", shape = "")+
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.125))+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        legend.position = "bottom",
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))
ggsave("report/plot2.png")
plot2_df <- NULL

#####################################################################################################
######## Figure 3: Non-Western Origin Shares in Technological Fields of Western countries ###########
#####################################################################################################

#### specify parameters
COUNTRIES <- c("US", "CA",
               "CH", 
               "GB", "FR", "DE", "IT", 
               "ES", "NL", "SE", "DK", "AT"#, "BE"
               )
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")

#### create the data
plot3_df <- non_western_tech(dat = inv_dat,
                             countries = COUNTRIES, 
                             origins = NON_WESTERN_ORIGIN,
                             start_year = 1980, end_year = 2015,
                             MA5 = TRUE, min_inventors = 30, min_years = 10)
#### test/sanity check
if(length(unique(plot3_df$country)) == length(COUNTRIES)){
  print("Data for Figure 3 successfully created")}else{
    warning("Number of countries in the created data does not correspond to function input.")}

#### visualize the data
# ggplot(plot3_df, aes(x = p_year, y = share, color = country))+
#   geom_line()+
#   facet_wrap(.~ tech_group) +
#   labs(x = "Year", y = "Aggregate Share of Non-Western Ethnic Origins") +
#   scale_y_continuous(labels = scales::percent, limits = c(0, 0.4)) +
#   theme(panel.background = element_blank(),
#         panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
#         legend.position = "bottom",
#         axis.line = element_line(),
#         axis.title = element_text(face="bold",size=10))

#### Add country name
plot3_df$country_name <- countrycode(plot3_df$country, "iso2c", "country.name.en")

##################################
######## Save the datasets #######
##################################

# for (i in seq(2,3)){
#   dat_name <- paste0("plot", i, "_df")
#   write.csv(get(dat_name), 
#             paste0("report/", dat_name, ".csv"),
#             row.names = FALSE)
#   print(paste("Data for plot", i, "saved."))
# }
write.csv(x = plot3_df, file = "report/plot3_df.csv", row.names = FALSE)
print("Data creation completed.")


