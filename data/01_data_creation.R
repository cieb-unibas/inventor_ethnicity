#######################################################################
# Description:    This script loads inventor data from the USPTO and  #
#                 the EPO to investigate the ethnicity of inventors   #
#                 across countries and over time.                     #
# Authors:        Matthias Niggli/CIEB University of Basel            #
# Last Revised:   1.12.2021                                          #
#######################################################################

##################################################
## Set directories, load packages and functions ##
##################################################

#### directories  -----------------------------------------------------------------
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_ethnicity"){
  print("Working directory corresponds to repository directory")}else{
    print("Make sure your working directory is the repository directory.")}
# setwd(paste0(getwd(), "/Innoscape-GitHub-Repos/inventor_ethnicity"))
DatDir <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"

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

##### stocks of immigrant and resident inventors per country ---------------------------------
# data is taken from: 
# Miguelez, Ernest, and Fink, Carsten,(2013) “Measuring the international mobility of inventors: a new database” WIPO Economic Research Working Paper No. 8.
immig_dat <- read.csv(
  paste0(
    DatDir,
    "/raw data/wipo_inventor_nationality_database/raw_files/13. Stocks of nationals, immigrants, residents, emigrants.csv")
)

#### Load helper functions --------------------------------------------
source("data/02_data_creation_funs.R")

#############################################################################
######## Figure 1: Non-Western Origin Shares in Western countries ###########
#############################################################################

#### specify parameters
COUNTRIES <- c("US", "CA", 
               "CH",
               "GB", "FR", "DE", "IT", 
               "ES", "NL", "SE", "DK", "AT"
               )
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")

#### create the data for plotting
plot1_df <- foreign_shares_fun(COUNTRIES, 
                               NON_WESTERN_ORIGIN, 
                               START_YEAR = 1985, END_YEAR = 2015)

#### test/sanity check:
if(length(unique(plot1_df$country)) == length(COUNTRIES)){
  print("Data for Figure 2 successfully created")}else{
    warning("Number of countries in the created data does not correspond to function input.")}

#### Add country name
plot1_df$country_name <- countrycode(plot1_df$country, "iso2c", "country.name.en")

#### Order the countries
tmp <- data.frame(country_name = unique(plot1_df$country_name), country_id = c(1,2,9,3,5,6,7,8,4,11,12,10))
plot1_df <- merge(plot1_df, tmp, all.x=TRUE, by = "country_name")
tmp <- NULL

#### visualize the data
ggplot(plot1_df, aes(x = p_year, y = share, color = origin))+
  facet_wrap(.~ country_name)+
  geom_line()+
  labs(x = "Year", y = "Shares of Non-Western Ethnic Origins", color = "", shape = "")+
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.125))+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        legend.position = "bottom",
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))
# ggsave("report/plot1.png")
# plot1_df <- NULL

#####################################################################################################
######## Figure 2: Non-Western Origin Shares in Technological Fields of Western countries ###########
#####################################################################################################

#### specify parameters
COUNTRIES <- c("US", "CA",
               "CH", 
               "GB", "FR", "DE", "IT", 
               "ES", "NL", "SE", "DK", "AT"
               )
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")

#### create the data
plot2_df <- non_western_tech(dat = inv_dat,
                             countries = COUNTRIES, 
                             origins = NON_WESTERN_ORIGIN,
                             start_year = 1980, end_year = 2015,
                             MA5 = TRUE, min_inventors = 30, min_years = 10)
#### test/sanity check
if(length(unique(plot2_df$country)) == length(COUNTRIES)){
  print("Data for Figure 3 successfully created")}else{
    warning("Number of countries in the created data does not correspond to function input.")}

#### Add country name
plot2_df$country_name <- countrycode(plot2_df$country, "iso2c", "country.name.en")

#### visualize the data
ggplot(plot2_df, aes(x = p_year, y = share, color = country_name))+
  geom_line()+
  facet_wrap(.~ tech_group) +
  labs(x = "Year", y = "Aggregate Share of Non-Western Ethnic Origins") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.4)) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        legend.position = "bottom",
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))

#### Order the data -> sorted in plot.js plot
plot2_df <- plot2_df %>%
  arrange(country_name)

plot2_df %>% write.csv2(paste0(getwd(), "/report/plot2_df.csv"))


##############################################
######## Figure 3: Immigrant Stocks ##########
##############################################

#### subset to the time window and countries of the analysis
TIME_WINDOW <- range(immig_dat$prio_year)
plot3_df <- immig_dat %>% 
  filter(iso_alpha2_code %in% unique(plot1_df$country) &
           prio_year %in% TIME_WINDOW)

#### select the data
plot3_df <- plot3_df %>% 
  group_by(iso_alpha2_code) %>% arrange(prio_year) %>%
  summarize(immigrant_difference = diff(immigrants)) %>%
  merge(plot3_df, all.x = TRUE, by = "iso_alpha2_code") %>%
  select(iso_alpha2_code, prio_year, immigrants, immigrant_difference) %>%
  rename(year = prio_year)
            
#### Add country name
plot3_df$country_name <- countrycode(plot3_df$iso_alpha2_code, "iso2c", "country.name.en")

#### visualize the data
ggplot(plot3_df, aes(x = country_name, y = immigrants, fill = as.factor(year)))+
  geom_col(position = "dodge")+
  labs(x = "Country", y = "Number of Immigrants") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        legend.position = "bottom",
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))

##################################
######## Save the datasets #######
##################################

for (i in seq(3)){
  dat_name <- paste0("plot", i, "_df")
  write.csv(get(dat_name),
            paste0("report/", dat_name, ".csv"),
            row.names = FALSE)
  print(paste("Data for plot", i, "saved."))
}


