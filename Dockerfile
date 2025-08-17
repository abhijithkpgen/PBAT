# Use a modern, general-purpose R base image
FROM rocker/r-ver:4.4.1

# 1. Install system dependencies required by your R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    zip \
    && rm -rf /var/lib/apt/lists/*

# 2. Install the renv package itself from CRAN
RUN R -e "install.packages('renv')"

# 3. Set up the app directory and copy the lockfile first
WORKDIR /app
COPY renv.lock .

# 4. Restore all packages from the lockfile. This is the key step
#    that ensures the exact versions you use locally are installed here.
RUN R -e "renv::restore()"

# 5. Copy the rest of your application code into the container
COPY . .

# 6. Install your PBAT package from the local source code
#    (repos = NULL ensures it uses the local files, not a remote repository)
RUN R -e "install.packages('.', repos = NULL, type = 'source')"

# Expose the port that Cloud Run will listen on
EXPOSE 8080

# The command to run when the container starts
CMD ["R", "-e", "PBAT::run_app()"]