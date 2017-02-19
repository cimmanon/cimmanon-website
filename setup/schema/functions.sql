BEGIN;

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Components
\*----------------------------------------------------------------------------------------------------*/

CREATE OR REPLACE FUNCTION add_component(info PROJECT_COMPONENTS, tags TEXT[]) RETURNS VOID AS $$
BEGIN
	-- main data
	INSERT INTO project_components
		(project, type, date_added, description, public, featured, archived)
	VALUES
		(info.project, info.type, info.date_added, info.description, info.public, info.featured, info.archived);

	-- tags
	INSERT INTO project_tags
		(project, type, date_added, tag)
	SELECT
		info.project,
		info.type,
		info.date_added,
		tag
	FROM
		unnest(tags) AS x(tag);
END;
$$ LANGUAGE plpgsql VOLATILE SET search_path FROM CURRENT;

---------------------------------------------------------------------

CREATE OR REPLACE FUNCTION edit_component(info PROJECT_COMPONENTS, tags TEXT[]) RETURNS VOID AS $$
BEGIN
	-- main data
	UPDATE project_components
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
	DELETE FROM project_tags
	WHERE
		project = info.project
		AND type = info.type
		AND date_added = info.date_added;

	-- add the new tags
	INSERT INTO project_tags
		(project, type, date_added, tag)
	SELECT
		info.project,
		info.type,
		info.date_added,
		tag
	FROM
		unnest(tags) AS x(tag);
END;
$$ LANGUAGE plpgsql VOLATILE SET search_path FROM CURRENT;

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Images
\*----------------------------------------------------------------------------------------------------*/

CREATE OR REPLACE FUNCTION update_images(info COMPONENT_IDENTITY, _featured TEXT, _delete TEXT[]) RETURNS VOID AS $$
BEGIN
	-- unset the current featured image
	UPDATE project_images
	SET
		featured = false
	WHERE
		project = info.project
		AND type = info.type
		AND date_added = info.date_added;

	-- set the new featured image
	UPDATE project_images
	SET
		featured = true
	WHERE
		project = info.project
		AND type = info.type
		AND date_added = info.date_added
		AND filename = _featured;

	-- delete files
	DELETE FROM project_images
	WHERE
		project = info.project
		AND filename = any(_delete);
END;
$$ LANGUAGE plpgsql VOLATILE SET search_path FROM CURRENT;
