FROM openanalytics/r-base

LABEL maintainer="Tobias Verbeke <tobias.verbeke@openanalytics.eu>"

# Mise à jour et installation des dépendances système
RUN apt-get update && apt-get install -y --no-install-recommends \
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
    libmpfr-dev \
    libudunits2-dev \
    libnlopt-dev \
    cmake \
    libharfbuzz-dev \
    libfribidi-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Mise à jour finale des paquets
RUN apt-get update && apt-get upgrade -y && apt-get clean && rm -rf /var/lib/apt/lists/*

# Installation des packages R nécessaires
RUN R -e "install.packages(c( \
    'shiny', 'rmarkdown', 'ggplot2', 'plotly', 'stringr', 'Hmisc', 'xml2', \
    'shinythemes', 'htmlwidgets', 'httr', 'ngramr', 'dplyr', 'htmltools', \
    'DT', 'sortable', 'tidyverse', 'quanteda', 'rainette', 'wordcloud', \
    'readxl', 'writexl', 'quanteda.textplots', 'quanteda.textstats', \
    'FactoMineR', 'factoextra', 'ggpubr', 'ggrepel', 'paletteer', 'udpipe', \
    'openxlsx', 'ggthemes', 'tools', 'scales', 'RColorBrewer', 'bslib', \
    'tibble', 'parallel', 'forcats', 'esquisse', 'lubridate' \
), dependencies=TRUE, repos='https://cloud.r-project.org/')"

# Ajout de l'application Shiny
RUN mkdir -p /root/mendak
COPY mendak /root/mendak

# Configuration R
COPY Rprofile.site /usr/lib/R/etc/

# Port d'exposition
EXPOSE 3838

# Commande de lancement
CMD ["R", "-e", "shiny::runApp('/root/mendak')"]
