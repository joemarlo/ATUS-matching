
download_ATUS <- function(){
  path <- 'inputs/ATUS-2003-2021'
  dir.create(path)
  
  urls <- c(
    "https://www.bls.gov/tus/special.requests/atusresp-0321.zip",
    'https://www.bls.gov/tus/special.requests/atusrost-0321.zip',
    'https://www.bls.gov/tus/special.requests/atusact-0321.zip',
    'https://www.bls.gov/tus/special.requests/atussum-0321.zip',
    'https://www.bls.gov/tus/special.requests/atuswho-0321.zip',
    'https://www.bls.gov/tus/special.requests/atuscps-0321.zip'
  )
  file_names <- c(
    'atusresp_0321.dat',
    'atusrost_0321.dat',
    'atusact_0321.dat',
    'atussum_0321.dat',
    'atuswho_0321.dat',
    'atuscps_0321.dat'
  )
  
  purrr::walk2(urls, file_names, function(url, file_name){
    download.file(url, destfile = file.path(path, 'atus.zip'))
    unzip(file.path(path, 'atus.zip'), files = file_name, exdir = path)
    file.remove(file.path(path, 'atus.zip'))
  })
}
