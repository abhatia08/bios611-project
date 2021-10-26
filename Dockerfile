FROM rocker/verse
MAINTAINER Abhishek Bhatia <abhatia@unc.edu>
RUN apt update && apt-get -y update && apt-get install -y  libudunits2-dev libgdal-dev libgeos-dev libproj-dev
RUN R -e "install.packages(\"reshape2\")"
RUN R -e "install.packages(\"shiny\")"
RUN R -e "install.packages(\"tinytex\")"
RUN R -e "tinytex::install_tinytex()"
RUN R -e "install.packages(\"tidyverse\")"
RUN R -e "install.packages(\"jsonlite\")"
RUN R -e "install.packages(\"data.table\")"
RUN R -e "install.packages(\"plyr\")"
RUN R -e "install.packages(\"tidygeocoder\")"
RUN R -e "install.packages(\"usmap\")"
RUN R -e "install.packages(\"here\")"
RUN R -e "install.packages(\"fs\")"
RUN R -e "install.packages(\"janitor\")"
RUN R -e "install.packages(\"tidycensus\")"
RUN R -e "install.packages(\"labelled\")"
