
clean_demographics <- function(ATUS_30, atusrost_0320, atuscps_0320, atussum_0320, atusresp_0320){
  
  # pull demographics information -------------------------------------------
  
  # from CPS, check if there is an elder in the household
  # elder = parent or other relative who is >=1 year older than respondent
  elder_in_HH <- atusrost_0320 %>% 
    group_by(TUCASEID) %>% 
    arrange(TUCASEID, TULINENO) %>%
    mutate(resp_age = first(TEAGE),
           is_elder = TERRP %in% c(24, 26) & TEAGE >= (resp_age + 1)) %>%
    summarize(elder_in_HH = any(is_elder),
              .groups = 'drop') %>% 
    rename(ID = TUCASEID)
  
  # from CPS, check if person has partner (is married or is living with partner)
  # has_partner <- atuscps_0318 %>%
  #   group_by(TUCASEID) %>% 
  #   summarize(has_partner = any(PERRP %in% c(3, 13, 14)),
  #             .groups = 'drop') %>% 
  #   rename(ID = TUCASEID)
  
  # get occupation code and match to essential worker
  industry <- dplyr::select(atusresp_0320, ID = TUCASEID, industry = TEIO1ICD)
  industry$industry[industry$industry == -1] <- NA
  industry$industry <- case_when(
    nchar(industry$industry) == 2 ~ paste0('00', industry$industry),
    nchar(industry$industry) == 3 ~ paste0('0', industry$industry),
    nchar(industry$industry) == 4 ~ as.character(industry$industry)
  )
  industry <- industry %>%
    dplyr::left_join(essential_industries, by = c('industry' = 'Census_2012')) %>%
    tidyr::replace_na(list(essential_group = 5)) %>%
    rename(essential_industry = essential_group)
  
  # from CPS data, get race, marriage status, education, metropolitan status, and state 
  # data dictionary here: https://www2.census.gov/programs-surveys/cps/datasets/2021/basic/2021_Basic_CPS_Public_Use_Record_Layout_plus_IO_Code_list.txt
  CPS_vars <- atuscps_0320 %>%
    filter(TULINENO == 1) %>%   # filter so only person responding to ATUS is included
    select(TUCASEID, PTDTRACE, PEEDUCA, GESTFIPS, GEMETSTA, GTMETSTA) %>%
    mutate(
      race = case_when(
        PTDTRACE == 1 ~ 'white',
        PTDTRACE == 2 ~ 'black',
        PTDTRACE == 4 ~ 'asian',
        TRUE ~ 'other'
      ),
      education = case_when(
        PEEDUCA < 38 ~ "Did not graduate from HS",
        PEEDUCA == 39 ~ "HS",
        PEEDUCA %in% 40:42 ~ "Some college",
        PEEDUCA == 43 ~ "Bachelors",
        PEEDUCA == 44 ~ "Masters",
        PEEDUCA %in% 44:45 ~ "Doctoral"
      ),
      metropolitan = case_when(
        GEMETSTA == 1 ~ 'metropolitan',
        GEMETSTA == 2 ~ 'non-metropolitan',
        GTMETSTA == 1 ~ 'metropolitan',
        GTMETSTA == 2 ~ 'non-metropolitan',
        TRUE ~ 'NA'
      )
    ) %>% 
    left_join(FIPS[, c('Name', 'FIPS')], by = c(GESTFIPS = 'FIPS')) %>%
    select(ID = TUCASEID, race, education, state = Name, metropolitan) %>% 
    distinct()
  
  # from ATUS data, get weights, age, sex, children, income,  
  atus_vars <- atussum_0320 %>% 
    mutate(survey_weight_2020 = TU20FWGT,
           survey_weight_ex_2020 = TUFNWGTP,
           survey_weight = if_else(TUYEAR == 2020, 
                                   survey_weight_2020,
                                   survey_weight_ex_2020)) %>% 
    select(TUCASEID, survey_weight, survey_weight_2020, survey_weight_ex_2020, 
           age = TEAGE, sex = TESEX, age_youngest = TRYHHCHILD, 
           n_child = TRCHILDNUM, labor_force_status = TELFS, 
           partner_working = TESPEMPNOT, has_partner = TRSPPRES) %>%
    mutate(
      has_partner = has_partner %in% c(1, 2),
      age_youngest = ifelse(age_youngest == -1, NA, age_youngest),
      labor_force_status = case_when(
        labor_force_status == 1 ~ 'employed - at work',
        labor_force_status == 2 ~ 'employed - absent',
        labor_force_status == 3 ~ 'unemployed - on layoff',
        labor_force_status == 4 ~ 'unemployed - looking',
        labor_force_status == 5 ~ 'not in labor force',
        TRUE ~ 'NA'
      ),
      partner_working = case_when(
        partner_working == -1 ~ 'NA',
        partner_working == 1 ~ 'employed',
        partner_working == 2 ~ 'not employed'
      )) %>%
    left_join(distinct(atuscps_0320[, c('TUCASEID', 'HEFAMINC', 'HUFAMINC')]),
              by = 'TUCASEID')
  
  # recode family income and account for change in reporting code
  atus_vars <- atus_vars %>% 
    left_join(select(income.levels, HUFAMINC, HH.income.new), by = 'HUFAMINC') %>%
    left_join(select(income.levels, HEFAMINC, HH.income.new), by = 'HEFAMINC') %>% 
    mutate(HH.income = pmax(HH.income.new.x, HH.income.new.y, na.rm = TRUE)) %>% 
    select(-c('HEFAMINC', 'HUFAMINC', 'HH.income.new.x', 'HH.income.new.y')) %>% 
    rename(ID = TUCASEID,
           fam_income = HH.income) %>% 
    distinct()
  
  # final dataset of demographic variables
  demographic_vars <- atus_vars %>% 
    left_join(elder_in_HH, by = 'ID') %>%
    # left_join(has_partner, by = 'ID') %>% 
    left_join(industry, by = 'ID') %>% 
    left_join(CPS_vars, by = 'ID') %>% 
    semi_join(distinct(ATUS_30, ID), by = 'ID') %>% 
    left_join(atusresp_0320 %>% 
                select(ID = TUCASEID, date = TUDIARYDATE, day_of_week = TUDIARYDAY, holiday = TRHOLIDAY,
                       year = TUYEAR, TRTALONE, TRTALONE_WK, TESCHFT),
              by = 'ID') %>% 
    mutate(student = case_when(
      TESCHFT == 1 ~ 'Full-time',
      TESCHFT == 2 ~ 'Part-time',
      TRUE ~ 'No'
    )) %>% 
    select(-TESCHFT)
  
  return(demographic_vars)
}
