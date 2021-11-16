#########################################################################
# Description:    Functions for analyzing ethnic origins of inventors   #
# Authors:        Matthias Niggli/CIEB UniBasel                         #
# Date:           15.11.2021 (revised 11.11.2021)                       #
#########################################################################

#### ETHNIC ORIGIN COMPOSITION: -------------------------------------------
# calculates weighted sum for all ethnic origins at the country level

inv_comp_ctry <- function(df, country){
        
        annual_total <- filter(df, Ctry_code == country) %>%
                group_by(p_year) %>% summarise(total = n())
        
        tmp <- filter(df, Ctry_code == country) %>%
                group_by(p_year) %>% select(contains("prob")) %>%
                summarise_all(.funs = sum)
        
        tmp <- merge(tmp, annual_total, by = "p_year")
        tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
        
        tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total)
        tmp$origin <- gsub("prob_", "", tmp$origin)
        tmp$country <- country
        tmp$total <- NULL
        tmp <- tmp[, c("p_year", "country", "origin", "share")]
        
        return(tmp)
}


#### SPECIFIC ORIGIN SHARES: -------------------------------------------
# calculates the individual share of each selected ethnic background for different countries.
# function requires a vector of countries, a vector of the selected ethnic origins 
# and a start and end year as an inputs.

foreign_shares_fun <- function(COUNTRIES, ORIGIN, START_YEAR, END_YEAR){
        
        inv_origin_shares <- lapply(COUNTRIES, function(x) inv_comp_ctry(inv_dat, x))
        for (i in length(inv_origin_shares)) {
                inv_origin_shares[[i]]$country <- COUNTRIES[i]
        }
        inv_origin_shares <- bind_rows(inv_origin_shares)
        
        plot_data <- filter(inv_origin_shares, origin %in% ORIGIN & 
                                    p_year <= END_YEAR & p_year >= START_YEAR) %>%
                arrange(p_year)
                
        return(plot_data)
}

#### ORIGIN SHARES BY COUNTRY AND TECHFIELDS: -------------------------------------------

# (1) calculates the ethnic origin composition within technological groups and countries.
# function requires a data.frame of inventor data and a vector of countries to consider
inv_comp_tech <- function(df, country){
        
        annual_total <- filter(df, Ctry_code == country) %>%
                        group_by(tech_group_name, p_year) %>% summarise(total = n())
        
        tmp <- filter(df, Ctry_code == country) %>%
                        group_by(tech_group_name, p_year) %>% select(contains("prob")) %>%
                        summarise_all(.funs = sum)
        tmp <- merge(tmp, annual_total, by = c("tech_group_name", "p_year"))
        tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
        
        tmp <- gather(tmp, key = "origin", 
                      value = "share", -p_year, -total, -tech_group_name)
        
        tmp$origin <- gsub("prob_", "", tmp$origin)
        tmp$country <- country
        
        return(tmp)
}

# (2) calculates the aggreagte share of a selection of ethnic background per 
# technological group and country.
non_western_tech <- function(dat,
                             countries, origins, 
                             start_year, end_year,
                             MA5 = FALSE,
                             min_inventors = 30, min_years = 10){
        
        # get country composition by tech_group
        inv_origin_shares <- lapply(countries, function(x) inv_comp_tech(df = dat, x))
        names(inv_origin_shares) <- countries
        
        # calculate aggreagte share of selected origins per techgroup and country
        country_diff <- data.frame()
        for(i in 1:length(inv_origin_shares)){
                        tmp <- filter(inv_origin_shares[[i]], 
                                      origin %in% origins & total >= min_inventors)
                        tmp <- tmp %>% group_by(p_year, tech_group_name) %>% 
                                summarize(share = sum(share)) %>%
                                filter(p_year <= end_year & p_year >= start_year) %>%
                                mutate(country = names(inv_origin_shares)[i])
                        country_diff <- rbind(country_diff, tmp)
        }
        
        # subset to country-techgroup pairs with at least "min_years" of observations
        country_diff <- country_diff %>% 
                mutate(country_tech = paste0(country, tech_group_name)) 
        tmp <- country_diff %>%
                group_by(country_tech) %>% summarize(count = n()) %>%
                filter(count > min_years)
        tmp <- tmp$country_tech
        country_diff <- country_diff %>% filter(country_tech %in% tmp) %>%
                select(-country_tech)
        
        # if specified: calculate 5year moving average:
        if(MA5 == TRUE){
                ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}
                country_diff <- country_diff %>%
                        group_by(country, tech_group_name) %>%
                        arrange(p_year) %>%
                        mutate(five_y_ma_share = ma(share)) %>%
                        filter(p_year > (start_year + 4))
                country_diff$share <- NULL
                country_diff <- rename(country_diff, share = five_y_ma_share)}
        
        # arrange and clean data.frame
        country_diff <- country_diff %>% arrange(p_year)
        country_diff <- country_diff[, c("p_year", "country", "tech_group_name", "share")] %>%
                rename(tech_group = tech_group_name)
        
        return(country_diff)
        }


