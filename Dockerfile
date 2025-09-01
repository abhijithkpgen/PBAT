# Use the rocker/verse image which includes Tidyverse and common system dependencies
FROM rocker/verse:4.4.1

# Install any specific system dependencies NOT included in rocker/verse (if any)
# For your current setup, the ones listed are likely already in rocker/verse,
# but keeping them here is harmless.
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    && rm -rf /var/lib/apt/lists/*

# Install the 'remotes' package
RUN R -e "install.packages('remotes', repos = 'https://cran.rstudio.com/')"

# Copy your local package source code into the container
COPY . /usr/src/pbat_src

# Install all dependencies and the package itself
RUN R -e "remotes::install_local('/usr/src/pbat_src', dependencies = TRUE)"

# Expose the port
EXPOSE 8080

# The command to run when the container starts
CMD ["R", "-e", "PBAT::run_app()"]