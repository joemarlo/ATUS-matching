
download_ATUS <- function(){
  path <- 'inputs/ATUS-2003-2020'
  dir.create(path)
  
  urls <- c(
    "https://www.bls.gov/tus/special.requests/atusresp-0320.zip",
    'https://www.bls.gov/tus/special.requests/atusrost-0320.zip',
    'https://www.bls.gov/tus/special.requests/atusact-0320.zip',
    'https://www.bls.gov/tus/special.requests/atussum-0320.zip',
    'https://www.bls.gov/tus/special.requests/atuswho-0320.zip',
    'https://www.bls.gov/tus/special.requests/atuscps-0320.zip'
  )
  file_names <- c(
    'atusresp_0320.dat',
    'atusrost_0320.dat',
    'atusact_0320.dat',
    'atussum_0320.dat',
    'atuswho_0320.dat',
    'atuscps_0320.dat'
  )
  
  purrr::walk2(urls, file_names, function(url, file_name){
    download.file(url, destfile = file.path(path, 'atus.zip'))
    unzip(file.path(path, 'atus.zip'), files = file_name, exdir = path)
    file.remove(file.path(path, 'atus.zip'))
  })
}
