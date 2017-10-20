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

DROP TRIGGER IF EXISTS update_local_archive_path ON projects;
CREATE TRIGGER update_local_archive_path
	AFTER UPDATE ON projects
	FOR EACH ROW
	EXECUTE PROCEDURE update_local_archive_path();

---------------------------------------------------------------------

CREATE OR REPLACE FUNCTION project_images_unset_featured() RETURNS TRIGGER AS $$
BEGIN
	UPDATE project_images
	SET
		featured = false
	WHERE
		project = NEW.project
		AND type = NEW.type
		AND date_added = NEW.date_added
		AND featured = true;

	RETURN NEW;
END;
$$ LANGUAGE 'plpgsql' SET search_path FROM CURRENT;
COMMENT ON FUNCTION project_images_unset_featured() IS 'Retires the current featured image for a project';

DROP TRIGGER IF EXISTS project_images_unset_featured ON project_images;
CREATE TRIGGER project_images_unset_featured
	BEFORE INSERT OR UPDATE ON project_images
	FOR EACH ROW
	WHEN (NEW.featured = true)
	EXECUTE PROCEDURE project_images_unset_featured();
