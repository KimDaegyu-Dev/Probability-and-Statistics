FROM rocker/tidyverse:latest

RUN R -e "install.packages(c( \
    'infer', \
    'broom', \
    'car', \
    'psych', \
    'skimr', \
    'janitor', \
    'readxl', \
    'writexl' \
), repos='https://cloud.r-project.org')"
