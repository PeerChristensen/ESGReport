FROM rocker/shiny:4.2.3


RUN apt-get update -qq && \
    apt-get install -y -qq --no-install-recommends \
        libz-dev \
        libpoppler-cpp-dev \
        pandoc \
        curl

RUN curl -L http://bit.ly/google-chrome-stable -o google-chrome-stable.deb && \
    apt-get -y install ./google-chrome-stable.deb && \
    rm google-chrome-stable.deb

COPY www/ /www/
COPY src/ /src/
COPY templates/ /templates/
COPY indicators/ /indicators/
COPY app.R app.R

COPY install_packages.R /tmp/install_packages.R
RUN Rscript /tmp/install_packages.R

# Give write read/write permission
RUN chmod ugo+rwx /

#USER shiny

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]





