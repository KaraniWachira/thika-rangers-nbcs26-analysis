# Use the official rocker image optimized for Shiny Server
FROM rocker/shiny:4.3.3

# Install underlying Linux system dependencies required for data/pipeline packages
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy our custom network configuration into the container
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Clean out any boilerplate example applications provided by the base image
RUN rm -rf /srv/shiny-server/*

# Install the explicit R packages required for your pipeline and interface
RUN R -e "install.packages(c('shiny', 'bslib', 'tidyverse', 'targets'), repos='https://cloud.r-project.org/')"

# Copy your local R application source files into the default hosting directory
COPY . /srv/shiny-server/

# --- CRITICAL FOR TARGETS ---
# Ensure the background 'shiny' user owns EVERYTHING, including the _targets store,
# so that the app can read the cache without data permission blocks.
RUN chown -R shiny:shiny /srv/shiny-server \
    && chown -R shiny:shiny /var/log/shiny-server

# Expose the internal port to match our config file
EXPOSE 7860

# Execute the pre-installed application server binary on startup
CMD ["/usr/bin/shiny-server"]