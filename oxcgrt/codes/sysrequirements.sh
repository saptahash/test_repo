#!/bin/bash
sudo apt-get install libcurl4-openssl-dev
sudo apt install -y libudunits2-0 libudunits2-dev
sudo apt install libgdal-dev
Rscript -e 'install.packages("remotes")'
Rscript -e 'remotes::install_cran(c("countrycode", "feather", "rgeos", "ggrepel",
"ggthemes", "viridis", "tidyverse", "RcppRoll", "zoo", "haven","sf", "rnaturalearth", "rnaturalearthdata", "here"))'
