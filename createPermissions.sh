psql -U postgres -d effdis -c "CREATE ROLE dbeare WITH PASSWORD 'cum2brae';"
psql -U postgres -d effdis -c "GRANT ALL PRIVILEGES ON DATABASE aas_base to dbeare;"
psql -U postgres -d effdis -c "GRANT USAGE ON SCHEMA global TO dbeare;"
