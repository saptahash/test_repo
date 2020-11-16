#!/bin/bash
sudo apt-get install libcurl4-openssl-dev
sudo apt install -y libudunits2-0 libudunits2-dev
sudo apt install libgdal-dev
Rscript -e 'install.packages("remotes")'
Rscript -e 'remotes::install_cran(c("ellipsis","tidyverse", "devtools", "rgeos", "ggthemes", "zoo","sf", "rnaturalearth", "rnaturalearthdata","here", "scales", "lubridate","writexl","openxlsx","janitor"))'
Rscript -e 'install_version("dplyr", version = "1.0.1", repos = "http://cran.us.r-project.org")'
Rscript -e 'devtools::install_github("tidyverse/tibble")'
