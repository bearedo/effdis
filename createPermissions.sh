
su - postgres


psql  -d effdis -c "GRANT ALL PRIVILEGES ON DATABASE effdis to dbeare;"
psql  -d effdis -c "GRANT USAGE ON SCHEMA public TO dbeare;"
psql -d effdis -c "select 'grant select on '||schemaname||'.'||tablename||' to dbeare;' from pg_tables where schemaname in ('public');"
psql -d effdis -c "select 'grant select on '||schemaname||'."'||viewname||'"to dbeare;' from pg_views where schemaname in ('public');"




psql -d effdis -c "CREATE USER lkell WITH PASSWORD 'Lkell$2o13';"
psql -d effdis -c "GRANT ALL PRIVILEGES ON DATABASE effdis to lkell;"


