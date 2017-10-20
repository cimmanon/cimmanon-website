BEGIN;

CREATE SCHEMA IF NOT EXISTS portfolio;

SET search_path to portfolio,public;

\ir schema/tables.sql
\ir schema/writeable_views.sql
\ir schema/triggers.sql
\ir schema/functions.sql
\ir data/projects.sql
