#!/bin/bash
sudo apt-get install libcurl4-openssl-dev
sudo apt install -y libudunits2-0 libudunits2-dev
sudo apt install libgdal-dev
Rscript -e 'install.packages("remotes")'
Rscript -e 'remotes::install_cran(c("ggplot2", "tidyr", "readr", "lubridate", "scales"))'
Rscript -e 'remotes::install_url("https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_1.0.1.tar.gz")'
