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
    && /usr/bin/Rscript -e "install.packages('rjson', dependencies=TRUE, repos='http://cran.rstudio.com/') "\
	&& /usr/bin/Rscript -e "install.packages('dplyr', dependencies=TRUE, repos='http://cran.rstudio.com/')" \
	&& /usr/bin/Rscript -e "install.packages('reticulate', dependencies=TRUE, repos='http://cran.rstudio.com/')" \
	&& /usr/bin/Rscript -e "install.packages('optparse', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# STAGE 2 - Python and python packages for S3 functionality
FROM stage1 as stage2
RUN apt update && apt -y install python3 python3-dev python3-pip python3-venv python3-boto3

# STAGE 3 set up I/O directories, copy geobamdata installer and R script
FROM stage2 as stage3
COPY ./prediagnostics/ /app/prediagnostics/

# STAGE 4 - Execute algorithm
FROM stage3 as stage4
LABEL version="1.0" \
	description="Containerized prediagnostics module." \
	"confluence.contact"="ntebaldi@umass.edu" \
	"algorithm.contact"="cjgleason@umass.edu"
ENTRYPOINT [ "/usr/bin/Rscript",  "/app/prediagnostics/run_prediagnostics.R" ]