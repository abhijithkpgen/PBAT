# Use a modern, general-purpose R base image
FROM rocker/r-ver:4.4.1

# 1. Install system dependencies that common R packages need on Linux
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    zip \
    libfontconfig1-dev \
    libharfbuzz-dev \
    && rm -rf /var/lib/apt/lists/*

# 2. Install the renv package itself
RUN R -e "install.packages('renv')"

# 3. Prepare the app directory
WORKDIR /app

# 4. Copy the lockfile. This tells renv what to install.
COPY renv.lock .

# 5. Restore all packages from the lockfile. This is the crucial step.
#    It ensures the exact versions you use locally are installed here.
RUN R -e "renv::restore()"

# 6. Copy the rest of your application code
COPY . .

# 7. Install your PBAT package itself into the renv library
RUN R -e "renv::install('.')"

# Expose the port that Cloud Run will listen on
EXPOSE 8080

# The command to run when the container starts
CMD ["R", "-e", "PBAT::run_app()"]