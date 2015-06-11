## Script to create spatially enabled databases for aas database project ### 

psql -U postgres createdb aas_base
psql -U postgres -d aas_base -f /usr/share/postgresql/9.1/contrib/postgis-2.0/postgis.sql
psql -U postgres -d aas_base -f /usr/share/postgresql/9.1/contrib/postgis-2.0/spatial_ref_sys.sql
psql -U postgres -d aas_base -f /usr/share/postgresql/9.1/contrib/postgis-2.0/postgis_comments.sql

# with raster support:

psql -U postgres -d aas_base -f /usr/share/postgresql/9.1/contrib/postgis-2.0/rtpostgis.sql
psql -U postgres -d aas_base -f /usr/share/postgresql/9.1/contrib/postgis-2.0/raster_comments.sql

# with topology support:

psql -U postgres -d aas_base -f /usr/share/postgresql/9.1/contrib/postgis-2.0/topology.sql
psql -U postgres -d aas_base -f /usr/share/postgresql/9.1/contrib/postgis-2.0/topology_comments.sql
