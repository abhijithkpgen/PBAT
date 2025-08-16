# Use a modern, general-purpose R base image
FROM rocker/r-ver:4.4.1

# 1. Install system dependencies required by your R packages
# This ensures packages like 'curl', 'xml2', and 'zip' can be installed
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    zip \
    && rm -rf /var/lib/apt/lists/*

# 2. Set up the working directory and copy your application code into the container
WORKDIR /app
COPY . .

# 3. Install the 'remotes' package, which is used to install dependencies
RUN R -e "install.packages('remotes', repos = 'https://cran.rstudio.com/')"

# 4. Install all R package dependencies listed in your DESCRIPTION file
# This is the most reliable way to ensure a consistent environment.
RUN R -e "remotes::install_deps(dependencies = TRUE)"

# 5. Install your PBAT package from the local source code we just copied
RUN R -e "remotes::install_local()"

# Expose the port that Cloud Run will listen on
EXPOSE 8080

# The command to run when the container starts.
# This calls the run_app() function from your newly installed package.
CMD ["R", "-e", "PBAT::run_app()"]