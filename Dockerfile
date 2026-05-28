# Use the official rocker image optimized for Shiny Server
FROM rocker/shiny:4.3.3

# Install underlying Linux system dependencies required for data/pipeline packages
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set the internal working directory to the default server root
WORKDIR /srv/shiny-server

# Copy our custom web server network configuration into the container
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Clean out any boilerplate example applications provided by the base image
RUN rm -rf /srv/shiny-server/*

# Install the explicit R packages required for your pipeline and interface
RUN R -e "install.packages(c('shiny', 'bslib', 'tidyverse', 'targets', 'imager','gt'), repos='https://cloud.r-project.org/')"

# Copy your ENTIRE local project directory layout into the container
# This copies _targets.R, the R/ folder, and the shiny/ folder perfectly
COPY . /srv/shiny-server/

# --- THE FIX FOR TARGETS + SHINY SUBFOLDER ---
# Move the contents of your shiny/ folder directly up to the server root 
# so Shiny Server sees it immediately, while keeping it in the same directory as _targets.R
RUN cp -r /srv/shiny-server/shiny/* /srv/shiny-server/

# Ensure the background 'shiny' user owns everything inside the working environment
RUN chown -R shiny:shiny /srv/shiny-server \
    && chown -R shiny:shiny /var/log/shiny-server

# Expose the internal port to match our config file
EXPOSE 7860

# Execute the pre-installed application server binary on startup
CMD ["/usr/bin/shiny-server"]