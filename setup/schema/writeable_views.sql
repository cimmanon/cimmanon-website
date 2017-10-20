DROP VIEW IF EXISTS project_components_with_tags CASCADE;
CREATE OR REPLACE VIEW project_components_with_tags AS
SELECT
	project
	, type
	, date_added
	, description
	, public
	, featured
	, archived
	, array_agg(tag :: TEXT) AS tags
FROM
	project_components
	LEFT JOIN project_tags USING (project, type, date_added)
GROUP BY
	project
	, type
	, date_added
;

CREATE OR REPLACE FUNCTION project_components_with_tags_update() RETURNS TRIGGER AS $$
BEGIN
	-- placing DELETE up front so we can exit early, allowing INSERT/UPDATE to share insert queries for tags
	IF TG_OP ='DELETE' THEN
		DELETE FROM project_components WHERE (project, type, date_added) = (OLD.project, OLD.type, OLD.date_added);

		RETURN NULL;
	ELSEIF TG_OP ='INSERT' THEN
		INSERT INTO project_components
			(project, type, date_added, description, public, featured, archived)
		VALUES
			(NEW.project, NEW.type, NEW.date_added, NEW.description, NEW.public, NEW.featured, NEW.archived);
	ELSEIF TG_OP = 'UPDATE' THEN
		IF (NEW.project, NEW.type, NEW.date_added, NEW.description, NEW.public, NEW.featured, NEW.archived) IS DISTINCT FROM (OLD.project, OLD.type, OLD.date_added, OLD.description, OLD.public, OLD.featured, OLD.archived) THEN
			UPDATE project_components
			SET
				project = NEW.project
				, type = NEW.type
				, date_added = NEW.date_added
				, description = NEW.description
				, public = NEW.public
				, featured = NEW.featured
				, archived = NEW.archived
			WHERE
				(project, type, date_added) = (OLD.project, OLD.type, OLD.date_added);
		END IF;

		IF NEW.tags IS DISTINCT FROM OLD.tags THEN
			DELETE FROM project_tags WHERE (project, type, date_added) = (NEW.project, NEW.type, NEW.date_added);
		END IF;
	END IF;

	IF array_length(NEW.tags, 1) > 0 AND (TG_OP ='INSERT' OR NEW.tags IS DISTINCT FROM OLD.tags) THEN
		INSERT INTO project_tags
			(project, type, date_added, tag)
		SELECT
			NEW.project
			, NEW.type
			, NEW.date_added
			, tag
		FROM
			unnest(NEW.tags) AS x(tag);
	END IF;

	RETURN NEW;
END;
$$ LANGUAGE plpgsql VOLATILE SET search_path FROM CURRENT;

DROP TRIGGER IF EXISTS project_components_with_tags_update ON project_components_with_tags;
CREATE TRIGGER project_components_with_tags_update
	INSTEAD OF INSERT OR UPDATE OR DELETE ON project_components_with_tags
	FOR EACH ROW EXECUTE PROCEDURE project_components_with_tags_update();
