FROM inseefrlab/onyxia-rstudio:latest

RUN quarto install tinytex && \
    git clone https://github.com/ThomasFaria/Quarterly_GFS_revisions.git && \
    cd Quarterly_GFS_revisions && \
    install2.r renv && \
    Rscript -e "renv::restore()" && \
    chown -R ${USERNAME}:${GROUPNAME} ${HOME}