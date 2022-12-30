FROM inseefrlab/onyxia-rstudio:latest

ARG QUARTO_VERSION="1.3.56"
ARG QUARTO_DL_URL="https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb"

RUN quarto install tool tinytex && \
    wget -q ${QUARTO_DL_URL} -O quarto.deb && \
    sudo dpkg -i quarto.deb && \
    quarto check install && \
    rm quarto.deb && \
    git clone https://github.com/ThomasFaria/Quarterly_GFS_revisions.git && \
    cd Quarterly_GFS_revisions && \
    install2.r renv && \
    # Rscript -e "renv::restore()" && \
    chown -R ${USERNAME}:${GROUPNAME} ${HOME}