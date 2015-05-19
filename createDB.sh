###### Script to build postgresql and postgis databases #####

####### Set up repositories ################################

sudo apt-get install python-software-properties
sudo apt-add-repository ppa:sharpie/for-science
sudo apt-add-repository ppa:sharpie/postgis-stable
sudo apt-add-repository ppa:ubuntugis/ubuntugis-unstable
sudo apt-get update
sudo apt-get install postgresql-9.1-postgis2

# Build Postgresql and Postgis

sudo apt-get install build-essential postgresql-9.1 postgresql-server-dev-9.1 libxml2-dev proj libjson0-dev xsltproc docbook-xsl docbook-mathml gettext postgresql-contrib-9.1 pgadmin3

# For server

sudo apt-get install python-software-properties

# Server and desktop

sudo apt-add-repository ppa:olivier-berten/geo
sudo apt-get update
sudo apt-get install libgdal1-dev

# Install gdal

sudo apt-add-repository ppa:ubuntugis/ubuntugis-unstable
sudo apt-get update
sudo apt-get install gdal-bin python-gdal

# Verify installation

gdal-config --version

# Install GEOS 3.3.x

sudo apt-get install g++ ruby ruby1.8-dev swig swig2.0 #''--- added to other instructions, not installed by default in 12.04 & required for this make
sudo wget http://download.osgeo.org/geos/geos-3.4.2.tar.bz2
sudo tar xvfj geos-3.4.2.tar.bz2
cd geos-3.4.3
sudo ./configure --enable-ruby --prefix=/usr

sudo make
sudo make install
cd ..

# Confirm installation

geos-config --version

#3.4.2

sudo apt-get install libproj-dev

# Build PostGIS

sudo wget http://postgis.refractions.net/download/postgis-2.0.0.tar.gz
sudo tar xfvz postgis-2.0.0.tar.gz
cd postgis-2.0.0
sudo ./configure --with-gui

# Finish installation

make
sudo make install
sudo ldconfig
sudo make comments-install

# Make sure command line tools work

sudo ln -sf /usr/share/postgresql-common/pg_wrapper /usr/local/bin/shp2pgsql
sudo ln -sf /usr/share/postgresql-common/pg_wrapper /usr/local/bin/pgsql2shp
sudo ln -sf /usr/share/postgresql-common/pg_wrapper /usr/local/bin/raster2pgsql





    
    
    
    
