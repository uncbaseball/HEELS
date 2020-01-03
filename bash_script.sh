sudo apt-get -y update
sudo chown -R ubuntu /etc/apt
sudo apt-key adv -keyserver keyserver.ubuntu.com -recv-keys E084DAB9
sudo add-apt-repository 'deb http://cran.rstudio.com/bin/linux/ubuntu trusty/'
sudo apt-get -y update
sudo apt-get install -y --force-yes r-base-core
sudo apt-get -y update
sudo chown -R ubuntu /etc/apt
sudo apt-key adv -keyserver keyserver.ubuntu.com -recv-keys E084DAB9
sudo add-apt-repository 'deb http://cran.rstudio.com/bin/linux/ubuntu trusty/'
sudo apt-get -y update
sudo apt-get install -y --force-yes r-base-core
sudo apt-get -y update
sudo chown -R ubuntu /etc/apt
sudo apt-key adv -keyserver keyserver.ubuntu.com -recv-keys E084DAB9
sudo add-apt-repository 'deb http://cran.rstudio.com/bin/linux/ubuntu trusty/'
sudo apt-get -y update
sudo apt-get install -y --force-yes r-base-core
sudo su -\-c "R -e \"install.packages( c('Rcpp', 'lattice' ,'shinyWidgets' ,'digest' ,'utf8' ,'mime' ,'R6' ,'cellranger' ,'backports' ,'httr' ,'pillar' ,'rlang' ,'lazyeval' ,'curl' ,'readxl' ,'rstudioapi' ,'data.table' ,'Matrix' ,'devtools' ,'htmlwidgets' ,'munsell' ,'broom' ,'compiler' ,'httpuv' ,'modelr' ,'pkgconfig' ,'htmltools' ,'tidyselect' ,'viridisLite' ,'fansi' ,'crayon' ,'withr' ,'later' ,'grid' ,'jsonlite' ,'xtable' ,'gtable' ,'git2r' ,'magrittr' ,'scales' ,'cli' ,'stringi' ,'promises' ,'tools' ,'glue' ,'ramazon' ,'hms' ,'yaml' ,'colorspace' ,'memoise' ,'bindr' ,'haven' ,'stats' ,'graphics' ,'grDevices' ,'utils' ,'datasets' ,'methods' ,'base' ,'plotly' ,'shiny' ,'rvest' ,'xml2' ,'lubridate' ,'plyr' ,'png' ,'RMySQL' ,'DBI' ,'bindrcpp' ,'default' ,'assertthat' ,'mgcv' ,'nlme' ,'forcats' ,'stringr' ,'dplyr' ,'purrr' ,'readr' ,'tidyr' ,'tibble' ,'ggplot2' ,'tidyverse' ) , repos = 'http://cran.rstudio.com/', dep = TRUE)\""
echo 'R installed'
sudo apt-get install -y gdebi-core
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.8.920-amd64.deb
sudo gdebi --non-interactive shiny-server-1.5.8.920-amd64.deb

sudo chown -R ubuntu /srv/
rm -Rf /srv/shiny-server/index.html
rm -Rf /srv/shiny-server/sample-apps
