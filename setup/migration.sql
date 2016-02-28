\set ON_ERROR_STOP true

BEGIN;

\echo '-----------------------------------------------------------'
\echo 'Migrating portfolio schema to the latest version...'
\echo '-----------------------------------------------------------'

\ir migration/1_archive_bool_to_text.sql
\ir migration/2_project_components_featured.sql

\echo '-----------------------------------------------------------'
\echo 'Migration completed successfully!'
\echo '-----------------------------------------------------------'

COMMIT;
