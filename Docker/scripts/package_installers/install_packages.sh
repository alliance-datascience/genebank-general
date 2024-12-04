
apt update -y
apt install sudo -y
sudo apt-get install build-essential -y
apt-get install wget -y
apt-get install gfortran -y
sudo apt-get update -qq
apt-get install -yq --no-install-recommends software-properties-common dirmngr
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt-get -y install --no-install-recommends r-base
sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable -y
sudo apt-get update -y
sudo apt-get -y install --no-install-recommends libgdal-dev libgeos-dev libproj-dev libudunits2-dev libxml2-dev \
    libcairo2-dev \
    libgit2-dev \
    default-libmysqlclient-dev \
    libpq-dev \
    libsasl2-dev \
    libsqlite3-dev \
    libssh2-1-dev \
    libxtst6 \
    libcurl4-openssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    unixodbc-dev \
    libsodium-dev\
    libssl-dev