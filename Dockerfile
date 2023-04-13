FROM rocker/shiny:4.2.3

# Install system requirements for index.R as needed
#RUN apt-get update && apt-get install -y 

#RUN apt-get update -qq && \
#    apt-get install -y -qq --no-install-recommends \
#        libz-dev \
#        libpoppler-cpp-dev \
#        pandoc \
#        curl

RUN sudo apt clean
RUN sudo apt-get update --allow-unauthenticated --allow-insecure-repositories
RUN apt-get update -qq && apt-get -y --allow-unauthenticated install  \
	chromium-browser

RUN install2.r pagedown pdftools

#RUN curl -L http://bit.ly/google-chrome-stable -o google-chrome-stable.deb && \
#    apt-get -y install ./google-chrome-stable.deb && \
#    rm google-chrome-stable.deb


#COPY google-chrome google-chrome
#COPY google-chrome /usr/local/bin/
#chmod u+x google-chrome

COPY www/ /www/
COPY src/ /src/
COPY templates/ /templates/
COPY indicators/ /indicators/
COPY app.R app.R

COPY install_packages.R /tmp/install_packages.R
RUN Rscript /tmp/install_packages.R

#RUN install2.r --error --deps TRUE pagedown

#COPY Rprofile.site /etc/R
#RUN install2.r --error --skipinstalled \
#    shiny 

#COPY ./app/* /srv/shiny-server/
#COPY ./app ./app

# Give write read/write permission
RUN chmod ugo+rwx /

#USER shiny

EXPOSE 3838

# https://stackoverflow.com/questions/66392202/run-shiny-server-on-different-port-than-3838

#ENTRYPOINT ["/usr/bin/shiny-server"]

# run app on container start
CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]





