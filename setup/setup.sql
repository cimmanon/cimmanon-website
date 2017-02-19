BEGIN;

CREATE SCHEMA IF NOT EXISTS portfolio;

set search_path to portfolio,public;

\ir schema/tables.sql
\ir schema/triggers.sql
\ir schema/functions.sql
\ir data/projects.sql
