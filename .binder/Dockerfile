FROM rocker/binder:3.6.0
LABEL maintainer='Serdar Balci'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/sbalci/histopathology-template/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-11-19/'));  install.packages('renv'); install.packages('remotes'); install.packages('pacman'); install.packages('rlang'); install.packages('here')"

RUN rm DESCRIPTION.1; exit 0
