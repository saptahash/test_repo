#!/bin/bash
sudo apt-get install libcurl4-openssl-dev
sudo apt install -y libudunits2-0 libudunits2-dev
sudo apt install libgdal-dev
Rscript -e 'install.packages("remotes")'
Rscript -e 'remotes::install_cran(c("ellipsis","tidyverse", "devtools", "rgeos", "ggthemes", "zoo","sf", "rnaturalearth", "rnaturalearthdata","here", "scales", "lubridate","writexl","openxlsx","janitor"))'
Rscript -e 'devtools::install_github("tidyverse/dplyr")'
