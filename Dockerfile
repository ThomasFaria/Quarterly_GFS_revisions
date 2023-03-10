FROM inseefrlab/onyxia-rstudio:ds-r4.2.3

WORKDIR ${HOME}/Quarterly_GFS_revisions
COPY . .

ARG QUARTO_VERSION="1.3.107"
ARG QUARTO_DL_URL="https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb"

RUN quarto install tool tinytex && \
    wget -q ${QUARTO_DL_URL} -O quarto.deb && \
    sudo dpkg -i quarto.deb && \
    quarto check install && \
    rm quarto.deb && \
    # Configure renv to use RSPM to download packages by default
    echo 'options(renv.config.repos.override = getOption("repos"))' >> ${R_HOME}/etc/Rprofile.site && \
    # Install R packages
    Rscript -e "renv::restore()" && \
    # Fix permissions
    chown -R ${USERNAME}:${GROUPNAME} ${HOME}
