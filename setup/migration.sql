\set ON_ERROR_STOP true

BEGIN;

\echo '-----------------------------------------------------------'
\echo 'Migrating portfolio schema to the latest version...'
\echo '-----------------------------------------------------------'

\ir migration/01_archive_bool_to_text.sql
\ir migration/02_project_components_featured.sql
\ir migration/03_foreign_key_on_delete_cascade.sql
\ir migration/04_switch_to_writeable_views.sql

\echo '-----------------------------------------------------------'
\echo 'Rebuilding replaceable objects...'
\echo '-----------------------------------------------------------'

\ir schema/writeable_views.sql
\ir schema/triggers.sql
\ir schema/functions.sql

\echo '-----------------------------------------------------------'
\echo 'Migration completed successfully!'
\echo '-----------------------------------------------------------'

--COMMIT;
