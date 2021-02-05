#!/bin/bash
sudo apt-get install libcurl4-openssl-dev
sudo apt install -y libudunits2-0 libudunits2-dev
sudo apt install libgdal-dev
#adding two additional installs to fix devtools bug
#apt-get -y build-dep libcurl4-gnutls-dev
#apt-get -y install libcurl4-gnutls-dev
Rscript -e 'install.packages(c("remotes"))'
Rscript -e 'remotes::install_cran(c("ellipsis","tidyverse", "rgeos", "ggthemes", "zoo","sf", "rnaturalearth", "rnaturalearthdata","here", "scales", "lubridate","writexl","openxlsx","janitor"))'
#Rscript -e 'remotes::install_url("https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_1.0.1.tar.gz")'
#Rscript -e 'remotes::install_github("tidyverse/tibble")'
