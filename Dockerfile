# Use a modern, general-purpose R base image
FROM rocker/r-ver:4.4.1

# 1. Install system dependencies required by R packages
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

# 3. Prepare the app directory and copy renv files
WORKDIR /app
COPY renv.lock .
COPY renv/.Rprofile .
COPY renv/activate.R .

# 4. Restore all packages from the lockfile. This is the crucial step.
RUN R -e "renv::restore()"

# 5. Copy the rest of your application source code
COPY . .

# 6. Install your PBAT package itself using a standard R command
#    This is the corrected step.
RUN R -e "install.packages('.', repos = NULL, type = 'source')"

# Expose the port that Cloud Run will listen on
EXPOSE 8080

# The command to run when the container starts
CMD ["R", "-e", "PBAT::run_app()"]