SET search_path TO portfolio,public;

BEGIN;

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Columns
\*----------------------------------------------------------------------------------------------------*/

-- add the new column
ALTER TABLE project_components ADD featured BOOL NOT NULL DEFAULT FALSE;
-- move the old "last" column to the end
ALTER TABLE project_components RENAME archived TO old_archived;
ALTER TABLE project_components ADD archived TEXT;
UPDATE project_components SET archived = old_archived;
ALTER TABLE project_components DROP COLUMN old_archived;

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Functions
\*----------------------------------------------------------------------------------------------------*/

CREATE OR REPLACE FUNCTION add_component(info PROJECT_COMPONENTS, tags TEXT[]) RETURNS VOID AS $$
BEGIN
	-- main data
	INSERT INTO portfolio.project_components
		(project, type, date_added, description, public, featured, archived)
	VALUES
		(info.project, info.type, info.date_added, info.description, info.public, info.featured, info.archived);

	-- tags
	INSERT INTO portfolio.project_tags
		(project, type, date_added, tag)
	SELECT
		info.project,
		info.type,
		info.date_added,
		tag
	FROM
		unnest(tags) AS x(tag);
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION edit_component(info PROJECT_COMPONENTS, tags TEXT[]) RETURNS VOID AS $$
BEGIN
	-- main data
	UPDATE portfolio.project_components
	SET
		description = info.description,
		public = info.public,
		featured = info.featured,
		archived = info.archived
	WHERE
		project = info.project
		AND type = info.type
		AND date_added = info.date_added;

	-- remove the old tags
	DELETE FROM portfolio.project_tags
	WHERE
		project = info.project
		AND type = info.type
		AND date_added = info.date_added;

	-- add the new tags
	INSERT INTO portfolio.project_tags
		(project, type, date_added, tag)
	SELECT
		info.project,
		info.type,
		info.date_added,
		tag
	FROM
		unnest(tags) AS x(tag);
END;
$$ LANGUAGE plpgsql VOLATILE;

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Defaults
\*----------------------------------------------------------------------------------------------------*/

-- set the components that used to be appear on the index page to be featured

UPDATE project_components AS c
SET
	featured = true
FROM
	(SELECT DISTINCT ON (project, type)
		MAX(project_components.date_added) OVER (PARTITION BY project) AS last_update,

		project_components.*,

		array_agg(tag :: TEXT) AS tags
	FROM
		portfolio.project_components
		JOIN portfolio.project_tags USING (project, type, date_added)
	GROUP BY
		project, type, date_added
	ORDER BY
		project,
		type,
		date_added DESC) AS f
WHERE
	c.project = f.project
	AND c.type = f.type
	AND c.date_added = f.date_added
;