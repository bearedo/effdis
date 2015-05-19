

# Script to install postgres and postgis

# Prerequisites #

sudo apt-get install build-essential postgresql-9.3 postgresql-server-dev-9.3 libgeos-c1 libgdal-dev 
libproj-dev libjson0-dev libxml2-dev libxml2-utils xsltproc docbook-xsl docbook-mathml

# Install postgis from source #

wget http://download.osgeo.org/postgis/source/postgis-2.1.7.tar.gz
tar xfz postgis-2.1.7.tar.gz
cd postgis-2.1.7

# A basic configuration for PostGIS 2.1, with raster and topology support #

./configure
make
sudo make install
sudo ldconfig
sudo make comments-install

# Enable the command-line tools to work from your shell: 


sudo ln -sf /usr/share/postgresql-common/pg_wrapper /usr/local/bin/shp2pgsql
sudo ln -sf /usr/share/postgresql-common/pg_wrapper /usr/local/bin/pgsql2shp
sudo ln -sf /usr/share/postgresql-common/pg_wrapper /usr/local/bin/raster2pgsql
