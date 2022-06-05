# library(tidyverse)
# library(rvest)

create_industries <- function(url_CDC, NAICS_2017, Census_2012_changes){

  # get essential industries table from CDC
  # url_CDC <- 'https://www.cdc.gov/vaccines/covid-19/categories-essential-workers.html'
  essential_industries <- rvest::read_html(url_CDC) %>% 
    rvest::html_table() %>%
    purrr::map(function(tbl) mutate(tbl, across(everything(), as.character))) %>% 
    bind_rows() %>% 
    setNames(c('NAICS_2017', 'name_2017', 'CISA_sector', 'vax_phase', 'workforce_category'))
  
  # expand the xxx codes to explicit codes
  essential_industries$NAICS_2017 <- stringr::str_replace_all(essential_industries$NAICS_2017, "x", ".")
  essential_industries$NAICS_2017 <- stringr::str_remove_all(essential_industries$NAICS_2017, "§")
  essential_industries$NAICS_2017 <- paste0("^", essential_industries$NAICS_2017, "$")
  # NAICS_2017 <- readxl::read_xlsx('inputs/2017_NAICS_Index_File.xlsx')
  NAICS_2017 <- NAICS_2017 %>% select(-DESCRIPTION) %>% distinct()
  essential_industries <- fuzzyjoin::regex_left_join(NAICS_2017, essential_industries, by = 'NAICS_2017')
  essential_industries <- essential_industries %>% 
    na.omit() %>% 
    select(NAICS_2017 = NAICS_2017.x,
           essential_group = vax_phase) %>% 
    mutate(essential_group = recode(essential_group, "1a" = 1, "1b" = 2, "1c" = 3))
  
  # crosswalk with the 2017 Census Industry codes
  Census_2017 <- readxl::read_xlsx('inputs/crosswalk_2017_Census_to_2017_NAICS.xlsx')
  colnames(Census_2017) <- c("Description", "Census_2017", "Restriction", "NAICS_2017")
  Census_2017 <- Census_2017[, c('Census_2017', 'NAICS_2017')]
  Census_2017 <- na.omit(Census_2017)
  Census_2017 <- distinct(Census_2017)
  
  ## a common issue is that the NAICS code will be "3118 exc. 311811" so we're 
  ##   going to naively remove all codes after "exc."
  Census_2017$NAICS_2017 <- stringr::str_remove_all(Census_2017$NAICS_2017, "[[:alpha:]].*$")
  
  ## unnest the codes. Where there are multiple codes in one row they will be converted to one per row
  Census_2017 <- tidytext::unnest_tokens(Census_2017, output = 'NAICS_2017', input = 'NAICS_2017')
  Census_2017 <- distinct(Census_2017)
  
  ## expand the xxx NAICS codes to explicit codes
  Census_2017$NAICS_2017 <- substr(paste0(Census_2017$NAICS_2017, "....."), 0, 6)
  Census_2017 <- fuzzyjoin::regex_left_join(NAICS_2017, Census_2017, by = 'NAICS_2017')
  Census_2017 <- dplyr::select(Census_2017, Census_2017, NAICS_2017 = NAICS_2017.x) %>% distinct()
  
  # crosswalk with the 2012 Census Industry codes
  # Census_2012_changes <- readxl::read_xlsx('inputs/crosswalk_2017_Census_to_2012_Census.xlsx',
  #                                          col_types = 'text')
  Census_to_NAICS <- left_join(Census_2017, Census_2012_changes, by = 'Census_2017') %>% 
    mutate(Census_2012 = if_else(is.na(Census_2012), Census_2017, Census_2012))
  
  # join back with list of essential industries
  essential_industries <- left_join(Census_to_NAICS, essential_industries, by = 'NAICS_2017') %>% 
    mutate(essential_group = if_else(is.na(essential_group), 4, essential_group))
  essential_industries %>%
    group_by(Census_2012) %>% 
    summarize(non_perfect_match = n_distinct(essential_group)) %>% 
    pull(non_perfect_match) %>% 
    {table(.) / sum(table(.))}
  # this tells us that 90% of the 2012 Census codes match to a single essential worker group
  # so we're going to map each Census_2012 group to the NAICS_2017 code (and consequently essential_group)
  #   by its average match. This loses a lot of information; there doesn't seem to be a better solution
  essential_industries <- essential_industries %>% 
    group_by(Census_2012) %>% 
    summarize(essential_group = mean(essential_group))
  
  return(essential_industries)
}



# write out
# write_csv(essential_industries, 'data/essential_industries.csv')
# rm(essential_industries, url_CDC, NAICS_2017, Census_to_NAICS, Census_2012_changes)
