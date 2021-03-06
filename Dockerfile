FROM rocker/verse
MAINTAINER Abhishek Bhatia <abhatia@unc.edu>
RUN apt update && apt install -y\
    libudunits2-dev\
    libgdal-dev\ 
    libgeos-dev\
    libproj-dev\
    texlive-base\
	texlive-binaries\
    texlive-latex-base\
	texlive-latex-recommended\
	texlive-pictures\
    texlive-latex-extra

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
RUN R -e "install.packages(\"ggExtra\")"
RUN R -e "install.packages(\"patchwork\")"
RUN R -e "install.packages(\"shiny\")"
RUN R -e "install.packages(\"shinythemes\")"
RUN R -e "install.packages(\"dplyr\")"
RUN R -e "install.packages(\"here\")"
RUN R -e "install.packages(\"ggExtra\")"
RUN R -e "install.packages(\"maps\")"
RUN R -e "install.packages(\"ggplot2\")"
RUN R -e "install.packages(\"patchwork\")"
RUN R -e "install.packages(\"viridis\")"
RUN R -e "install.packages(\"plyr\")"
RUN R -e "install.packages(\"jsonlite\")"
RUN R -e "install.packages(\"data.table\")"
RUN R -e "install.packages(\"tidyverse\")"
RUN R -e "install.packages(\"tidygeocoder\")"
RUN R -e "install.packages(\"usmap\")"
RUN R -e "install.packages(\"fs\")"
RUN R -e "install.packages(\"janitor\")"
RUN R -e "install.packages(\"tidycensus\")"
RUN R -e "install.packages(\"labelled\")"
RUN R -e "install.packages(\"ggthemes\")"
RUN R -e "install.packages(\"cowplot\")"

