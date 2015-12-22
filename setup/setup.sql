BEGIN;

CREATE SCHEMA IF NOT EXISTS portfolio;

set search_path to portfolio,public;

\i schema/projects.sql
\i data/projects.sql

