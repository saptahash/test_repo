#!/bin/bash
sudo apt-get install libcurl4-openssl-dev
sudo apt install -y libudunits2-0 libudunits2-dev
sudo apt install libgdal-dev
Rscript -e 'install.packages("remotes", "devtools")'
Rscript -e 'remotes::install_cran(c("ellipsis","tidyverse", "rgeos", "ggthemes", "zoo","sf", "rnaturalearth", "rnaturalearthdata","here", "scales", "lubridate","writexl","openxlsx","janitor"))'
Rscript -e 'devtools::install_url("https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_1.0.1.tar.gz")'
Rscript -e 'devtools::install_github("tidyverse/tibble")'
