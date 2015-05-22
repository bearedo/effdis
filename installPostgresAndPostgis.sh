

# Script to install postgres and postgis

sudo su -

sudo apt-get update

sudo apt-get install postgresql-9.3-postgis-2.1

# Import the Boundless GPG key:

wget -qO- https://apt.boundlessgeo.com/gpg.key | apt-key add -

# Add the OpenGeo Suite repository:

echo "deb https://apt.boundlessgeo.com/suite/v45/ubuntu/ trusty main" > /etc/apt/sources.list.d/opengeo.list

apt-get update

# Search for OpenGeo Suite packages to verify that the repository list is correct. If the command does not return any results, examine the output of the apt command for any errors or warnings.

apt-cache search opengeo

# To install typical server components:

    apt-get install opengeo-server

# To install typical client components:

    apt-get install opengeo-client

# To install typical client and server components:

    apt-get install opengeo

# Controlling the postgres server

sudo service postgresql start|stop|restart

