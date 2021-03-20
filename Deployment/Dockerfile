FROM rocker/shiny:3.5.1 
RUN apt-get -y install libcurl4-gnutls-dev libxml2-dev libssl-dev; \ 
rm -r /srv/shiny-server; \ 
mkdir -p /var/lib/shiny-server/bookmarks/shiny; \ 
sed -i 's/3838/3838 0.0.0.0/' /etc/shiny-server/shiny-server.conf 
RUN R -e "install.packages(pkgs=c('shiny','tidyverse','shinydashboard', 'xgboost', 'DT', 'plotly', 'tidymodels', 'ggridges', 'here'), repos='https://cran.rstudio.com/')" 

COPY app.R /srv/shiny-server/
COPY modules.R /srv/shiny-server/
COPY Final_Model.RDS /srv/shiny-server/data
COPY fight_data.csv /srv/shiny-server/data
COPY fighter_table.csv /srv/shiny-server/data
COPY future_card_predictions.csv /srv/shiny-server/data

EXPOSE 3838

RUN chmod -R +rx /srv/shiny-server/ 

USER shiny 
EXPOSE 3838 
CMD ["/usr/bin/shiny-server.sh"]