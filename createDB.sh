
/etc/init.d/postgresql restart
   
## Script to create spatially enabled database for effdis database project ### 

sudo -i -u postgres
createdb effdis
createlang plpgsql effdis
psql -d effdis -c "CREATE EXTENSION postgis;"
psql -d effdis -c "CREATE EXTENSION postgis_topology;"
#psql -d effdis -c "CREATE EXTENSION postgis_tiger_geocoder;"



    
    
    
    
