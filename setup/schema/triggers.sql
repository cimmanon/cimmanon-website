BEGIN;

CREATE OR REPLACE FUNCTION update_local_archive_path() RETURNS TRIGGER AS $$
BEGIN
	UPDATE project_components
	SET
		archived = replace(archived, OLD.slug, NEW.slug)
	WHERE
		project = NEW.project
		AND substring(archived from 1 for 1) = '/';
	RETURN NEW;
END;
$$ LANGUAGE 'plpgsql' SET search_path FROM CURRENT;
COMMENT ON FUNCTION update_local_archive_path() IS 'Updates the slug information in local paths';

-- TODO: move this to the next migration file
-- this is a legacy trigger name that needs to get dropped as well
DROP TRIGGER IF EXISTS roster_revision_checks ON projects;
DROP TRIGGER IF EXISTS update_local_archive_path ON projects;
CREATE CONSTRAINT TRIGGER update_local_archive_path AFTER UPDATE ON projects
	FOR EACH ROW EXECUTE PROCEDURE update_local_archive_path();
