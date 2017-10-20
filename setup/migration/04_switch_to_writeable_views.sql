BEGIN;

\echo 'Cleaning up objects that aren''t needed anymore thanks to writeable views...'

-- drop oddly named legacy trigger that came from a different project
DROP TRIGGER IF EXISTS roster_revision_checks ON projects;

-- drop newly removed functions
DROP FUNCTION IF EXISTS add_component(PROJECT_COMPONENTS, TEXT[]);
DROP FUNCTION IF EXISTS edit_component(PROJECT_COMPONENTS, TEXT[]);
