# STAGE 0 - Ubuntu packages and R repository
FROM ubuntu as stage0
RUN echo "America/New_York" | tee /etc/timezone \
	&& apt update \
	&& DEBIAN_FRONTEND=noninteractive apt install -y \
		build-essential \
		gcc \
		gfortran \
        locales \
		libcurl4-gnutls-dev \
		libfontconfig1-dev \
		libfribidi-dev \
		libgit2-dev \
		libharfbuzz-dev \
		libnetcdf-dev \
		libnetcdff-dev \
		libssl-dev \
		libtiff5-dev \
		libxml2-dev \
		tzdata \
		wget \
    && locale-gen en_US.UTF-8

# STAGE 1 - R and R packages
FROM stage0 as stage1
RUN apt-get update
RUN apt -y install \
		software-properties-common \
		dirmngr \
	&& . /etc/lsb-release \
	&& wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc \
	&& add-apt-repository -y "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" \
	&& apt install -y \
		r-base \
		r-base-dev \
	&& /usr/bin/Rscript -e "install.packages('RNetCDF', dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && /usr/bin/Rscript -e "install.packages('rjson', dependencies=TRUE, repos='http://cran.rstudio.com/')"\
	&& /usr/bin/Rscript -e "install.packages('dplyr', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# STAGE 2 set up I/O directories, copy geobamdata installer and R script
FROM stage1 as stage2
COPY ./prediagnostics/ /app/prediagnostics/

# STAGE 3 - Execute algorithm
FROM stage2 as stage3
LABEL version="1.0" \
	description="Containerized prediagnostics module." \
	"confluence.contact"="ntebaldi@umass.edu" \
	"algorithm.contact"="cjgleason@umass.edu"
ENTRYPOINT [ "/usr/bin/Rscript",  "/app/prediagnostics/run_prediagnostics.R" ]