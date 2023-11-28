# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# Install linux packages
# This needs revising if MAVISApp does not need spatial libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgdal-dev \
    gdal-bin \
    libproj-dev \
    proj-data \
    proj-bin \
    libgeos-dev

# Update linux packages
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Copy necessary files
COPY /app.R /app.R
COPY /modules /modules
COPY /data /data
COPY /R /R
COPY /www /www
COPY /renv.lock /renv.lock

# Install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'
RUN Rscript -e 'tinytex::install_tinytex()'

# Expose port
EXPOSE 8001

# Run app
CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 8001)"]
