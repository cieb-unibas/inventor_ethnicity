#########################################################################
# Description:    Functions for analyzing ethnic origins of inventors   #
# Authors:        Matthias Niggli/CIEB UniBasel                         #
# Date:           16.02.2021 (revised 11.11.2021)                       #
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
                                    p_year <= END_YEAR & p_year >= START_YEAR)
        
        return(plot_data)
}

#### ORIGIN SHARES BY COUNTRY AND TECHFIELDS: -------------------------------------------
# calculates the ethnic origin composition within techfields and countries.
# function requires a vector of countires and specification if techfields should be
# grouped to major techfields.

inv_comp_techfield <- function(df, country, grouping = FALSE){
        
        if(grouping == FALSE){
                annual_total <- filter(df, Ctry_code == country) %>%
                        group_by(tech_field, p_year) %>% summarise(total = n())
                
                tmp <- filter(df, Ctry_code == country) %>%
                        group_by(tech_field, p_year) %>% select(contains("prob")) %>%
                        summarise_all(.funs = sum)
                
                tmp <- merge(tmp, annual_total, by = c("tech_field", "p_year"))
                tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
                
                tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total, -tech_field)
        }else{
                annual_total <- filter(df, Ctry_code == country) %>%
                        group_by(tech_group_name, p_year) %>% summarise(total = n())
                
                tmp <- filter(df, Ctry_code == country) %>%
                        group_by(tech_group_name, p_year) %>% select(contains("prob")) %>%
                        summarise_all(.funs = sum)
                
                tmp <- merge(tmp, annual_total, by = c("tech_group_name", "p_year"))
                tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
                
                tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total, -tech_group_name)  
        }
        
        tmp$origin <- gsub("prob_", "", tmp$origin)
        tmp$country <- country
        
        return(tmp)
}

# calculates the (moving average of) the cumulative share of a selection of ethnic background for a specification of
# selected techfields and countries.

non_western_techfield <- function(countries, origins, techfields, start_year, 
                                  end_year, min_inventors = 30, MA_5 = FALSE){
        
        inv_origin_shares <- lapply(countries, function(x) inv_comp_techfield(inv_dat, x, grouping = FALSE))
        names(inv_origin_shares) <- countries
        
        country_diff <- data.frame()
        
        for(i in 1:length(inv_origin_shares)){
                tmp <- filter(inv_origin_shares[[i]], 
                              origin %in% origins & total >= min_inventors &
                                      tech_field %in% techfields)
                tmp <- tmp %>% group_by(p_year, tech_field) %>% summarize(share = sum(share)) %>%
                        filter(p_year <= end_year & p_year >= start_year) %>%
                        mutate(country = names(inv_origin_shares)[i])
                country_diff <- rbind(country_diff, tmp)
        }
        
        # calculate 5year rolling average:
        if(MA_5 == TRUE){
                ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}
                country_diff <- country_diff %>%
                        group_by(country, tech_field) %>%
                        arrange(p_year) %>%
                        mutate(five_y_ma_share = ma(share)) %>%
                        filter(p_year > (start_year + 4))}
        
        return(country_diff)}


