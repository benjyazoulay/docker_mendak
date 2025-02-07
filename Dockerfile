FROM openanalytics/r-base

MAINTAINER Tobias Verbeke "tobias.verbeke@openanalytics.eu"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    libxml2-dev \
    libgdal-dev \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the gallicagram app
RUN apt-get update && apt-get install -y \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
	libnlopt-dev \
	&& rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
	cmake \
	&& rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get upgrade --yes

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('ggplot2','plotly','stringr','Hmisc','xml2','shinythemes','htmlwidgets','httr','ngramr','dplyr','htmltools'), repos='https://cloud.r-project.org/')"

RUN R -e 'install.packages(c("DT", "sortable", "tidyverse", "quanteda", "rainette", "wordcloud", "readxl", "writexl", "quanteda.textplots", "quanteda.textstats", "FactoMineR", "factoextra", "ggpubr", "ggrepel", "paletteer", "udpipe", "openxlsx"))'

# copy the app to the image
RUN mkdir /root/mendak
COPY mendak /root/mendak

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/mendak')"]
