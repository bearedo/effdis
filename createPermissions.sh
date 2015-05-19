psql -U postgres -d effdis -c "CREATE USER dbeare WITH PASSWORD 'cum2brae';"
psql -U postgres -d effdis -c "GRANT ALL PRIVILEGES ON DATABASE effdis to dbeare;"
psql -U postgres -d effdis -c "GRANT USAGE ON SCHEMA public TO dbeare;"
psql -U postgres -d effdis -c "select 'grant select on '||schemaname||'.'||tablename||' to dbeare;' from pg_tables where schemaname in ('public');"
psql -U postgres -d effdis -c "select 'grant select on '||schemaname||'."'||viewname||'"to dbeare;' from pg_views where schemaname in ('public');"




psql -U postgres -d effdis -c "CREATE USER lkell WITH PASSWORD 'Lkell$2o13';"
psql -U postgres -d effdis -c "GRANT ALL PRIVILEGES ON DATABASE effdis to lkell;"


