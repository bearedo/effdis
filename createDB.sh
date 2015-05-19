
/etc/init.d/postgresql restart
   
## Script to create spatially enabled database for effdis database project ### 

sudo -i -u postgres
createdb effdis
psql -d effdis
CREATE EXTENSION postgis;
CREATE EXTENSION postgis_topology;

psql -U postgres -d effdis -f /usr/share/postgresql/9.1/contrib/postgis-2.0/postgis.sql
psql -U postgres -d effdis -f /usr/share/postgresql/9.1/contrib/postgis-2.0/spatial_ref_sys.sql
psql -U postgres -d effdis -f /usr/share/postgresql/9.1/contrib/postgis-2.0/postgis_comments.sql

# with raster support:

psql -U postgres -d effdis -f /usr/share/postgresql/9.1/contrib/postgis-2.0/rtpostgis.sql
psql -U postgres -d effdis -f /usr/share/postgresql/9.1/contrib/postgis-2.0/raster_comments.sql

# with topology support:

psql -U postgres -d effdis -f /usr/share/postgresql/9.1/contrib/postgis-2.0/topology.sql
psql -U postgres -d effdis -f /usr/share/postgresql/9.1/contrib/postgis-2.0/topology_comments.sql


    
    
    
    
