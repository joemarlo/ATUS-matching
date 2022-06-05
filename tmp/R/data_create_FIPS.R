
url_FIPS <- 'https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696'
create_fips <- function(url_FIPS){
  xml2::read_html(url_FIPS) %>% 
    rvest::html_nodes(xpath = '//table[contains(@class, "data")]') %>% 
    rvest::html_table() %>% 
    .[[1]] %>% 
    rename(State = 'Postal Code')
}
