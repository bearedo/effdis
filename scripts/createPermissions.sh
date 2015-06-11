
## File to create permissions and roles on aas_gis database
## File to create permissions and roles on aas_gis database

psql -U postgres -d aas_base -c "CREATE ROLE guest WITH PASSWORD 'sculpin';"
psql -U postgres -d aas_base -c "GRANT ALL PRIVILEGES ON DATABASE aas_base to guest;"
psql -U postgres -d aas_base -c "GRANT USAGE ON SCHEMA global TO guest;"

psql -U postgres -d aas_base -c "CREATE ROLE dbeare WITH PASSWORD 'cum2brae';"
psql -U postgres -d aas_base -c "GRANT ALL PRIVILEGES ON DATABASE aas_base to dbeare;"
psql -U postgres -d aas_base -c "GRANT USAGE ON SCHEMA global TO dbeare;"

psql -U postgres -d aas_base -c "CREATE USER worldfish WITH PASSWORD 'tope';"
psql -U postgres -d aas_base -c "GRANT ALL PRIVILEGES ON DATABASE aas_base to worldfish;"
psql -U postgres -d aas_base -c "GRANT USAGE ON SCHEMA global TO worldfish;"




psql -U postgres -d aas_base -c "CREATE ROLE sltan WITH PASSWORD '123456';"
psql -U postgres -d aas_base -c "GRANT ALL PRIVILEGES ON DATABASE aas_base to sltan;"
psql -U postgres -d aas_base -c "GRANT USAGE ON SCHEMA global TO sltan;"

psql -U postgres -d aas_base -c "CREATE ROLE steoh WITH PASSWORD '123456';"
psql -U postgres -d aas_base -c "GRANT ALL PRIVILEGES ON DATABASE aas_base to steoh;"
psql -U postgres -d aas_base -c "GRANT USAGE ON SCHEMA global TO steoh;"


psql -U postgres -d aas_base -c "CREATE ROLE nahmad WITH PASSWORD '123456';"
psql -U postgres -d aas_base -c "GRANT ALL PRIVILEGES ON DATABASE aas_base to nahmad;"
psql -U postgres -d aas_base -c "GRANT USAGE ON SCHEMA global TO nahmad;"


psql -U postgres -d aas_base -c "CREATE ROLE bcampbell WITH PASSWORD '123456';"
psql -U postgres -d aas_base -c "GRANT ALL PRIVILEGES ON DATABASE aas_base to bcampbell;"
psql -U postgres -d aas_base -c "GRANT USAGE ON SCHEMA global TO bcampbell;"


psql -U postgres -d aas_base -c "CREATE ROLE ccrissman WITH PASSWORD '123456';"
psql -U postgres -d aas_base -c "GRANT ALL PRIVILEGES ON DATABASE aas_base to ccrissman;"
psql -U postgres -d aas_base -c "GRANT USAGE ON SCHEMA global TO ccrissman;"

## Create groups

psql -U postgres -d aas_base -c "CREATE GROUP ccafs WITH USER dbeare,sltan,steoh,bcampbell,ccrissman;"

## File to create permissions and roles on aas_gis database

psql -U postgres -d aas_base -c "select 'grant select on '||schemaname||'.'||tablename||' to dbeare;' from pg_tables where schemaname in ('global');"
psql -U postgres -d aas_base -c "select 'grant select on '||schemaname||'."'||viewname||'" to dbeare;' from pg_views where schemaname in ('global');"
psql -U postgres -d aas_base -c "select 'grant select on '||schemaname||'.'||tablename||' to dbeare;' from pg_tables where schemaname in ('public');"
psql -U postgres -d aas_base -c "select 'grant select on '||schemaname||'."'||viewname||'"to dbeare;' from pg_views where schemaname in ('public');"

psql -U postgres -d aas_base -c "select 'grant select on '||schemaname||'.'||tablename||' to worldfish;' from pg_tables where schemaname in ('global');"
#psql -U postgres -d aas_base -c "select 'grant select on '||schemaname||'."'||viewname||'" to worldfish;' from pg_views where schemaname in ('global');"
psql -U postgres -d aas_base -c "select 'grant select on '||schemaname||'.'||tablename||' to worldfish;' from pg_tables where schemaname in ('public');"
#psql -U postgres -d aas_base -c "select 'grant select on '||schemaname||'."'||viewname||'"to worldfish;' from pg_views where schemaname in ('public');"
















