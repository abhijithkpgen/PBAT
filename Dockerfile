# Use a modern, general-purpose R base image
FROM rocker/r-ver:4.4.1

# Install system dependencies required by some of your R packages
# This ensures packages like 'curl' and 'xml2' can be installed
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    && rm -rf /var/lib/apt/lists/*

# Install the 'remotes' package, which is used to install packages from GitHub
RUN R -e "install.packages('remotes', repos = 'https://cran.rstudio.com/')"

# Install your PBAT package and all its dependencies directly from GitHub
# This single command reads your DESCRIPTION file and installs everything needed.
RUN R -e "remotes::install_github('abhijithkpgen/PBAT')"

# Expose the port that Cloud Run will listen on
EXPOSE 8080

# The command to run when the container starts.
# This sets the correct host and port, then calls the run_app() function from your package.
CMD ["R", "-e", "PBAT::run_app()"]
