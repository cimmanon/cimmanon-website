
/*----------------------------------------------------------------------------------------------------*\
                                                                      | Types
\*----------------------------------------------------------------------------------------------------*/

DO $$
BEGIN
	IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'label' AND typtype = 'd') THEN
		CREATE DOMAIN label AS TEXT
			CONSTRAINT label_not_empty_ck CHECK (VALUE != '')
			CONSTRAINT label_max_length_ck CHECK (char_length(VALUE) <= 150)
		;
	END IF;


	IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'slug' AND typtype = 'd') THEN
		CREATE DOMAIN slug as TEXT
--			CONSTRAINT slug_not_empty_ck CHECK (VALUE != '')
			CONSTRAINT slug_valid_characters_ck CHECK (value ~* '^[a-zA-Z0-9\-\._]+$');
	END IF;
END $$;

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Core Tables
\*----------------------------------------------------------------------------------------------------*/

CREATE TABLE projects (
	project LABEL NOT NULL,
	description TEXT NOT NULL,
	slug SLUG NOT NULL,
	url TEXT,
	featured BOOL NOT NULL DEFAULT true,

	PRIMARY KEY (project),
	UNIQUE (slug)
);

--------------------------------------------------------------------- | Project types

CREATE TABLE project_types (
	type LABEL NOT NULL PRIMARY KEY
);

CREATE TABLE project_components (
	project LABEL NOT NULL,
	type LABEL NOT NULL,
	date_added DATE NOT NULL DEFAULT NOW(),
	description TEXT NOT NULL,
	public BOOL NOT NULL default true,
	archived TEXT,

	PRIMARY KEY (project, type, date_added),
	FOREIGN KEY (project) REFERENCES projects (project) ON UPDATE CASCADE,
	FOREIGN KEY (type) REFERENCES project_types (type) ON UPDATE CASCADE
);
CREATE INDEX project_components_component_idx ON project_components (type);
CREATE INDEX project_components_year_idx ON project_components (extract(year FROM date_added) DESC);

CREATE TYPE component_identity AS (project LABEL, type LABEL, date_added DATE);

--------

CREATE OR REPLACE FUNCTION update_local_archive_path() RETURNS TRIGGER AS $$
BEGIN
	UPDATE portfolio.project_components
	SET
		archived = replace(archived, OLD.slug, NEW.slug)
	WHERE
		project = NEW.project
		AND substring(archived from 1 for 1) = '/';
	RETURN NEW;
END;
$$ LANGUAGE 'plpgsql';
COMMENT ON FUNCTION update_local_archive_path() IS 'Updates the slug information in local paths';

CREATE CONSTRAINT TRIGGER roster_revision_checks AFTER UPDATE ON projects
	FOR EACH ROW EXECUTE PROCEDURE update_local_archive_path();

--------------------------------------------------------------------- | Tags

CREATE TABLE tag_categories (
	category LABEL NOT NULL PRIMARY KEY
);

CREATE TABLE project_type_tags (
	type LABEL NOT NULL,
	tag LABEL NOT NULL,
	category LABEL NOT NULL,

	PRIMARY KEY (type, tag)
);
CREATE INDEX project_type_tags_tag_idx ON project_type_tags (tag);
CREATE INDEX project_type_tags_category_idx ON project_type_tags (category);

CREATE TABLE project_tags (
	project LABEL NOT NULL,
	type LABEL NOT NULL,
	date_added DATE NOT NULL,
	tag LABEL NOT NULL,

	PRIMARY KEY (project, type, date_added, tag),
	FOREIGN KEY (project, type, date_added) REFERENCES project_components (project, type, date_added) ON UPDATE CASCADE,
	FOREIGN KEY (type, tag) REFERENCES project_type_tags (type, tag) ON UPDATE CASCADE
);
CREATE INDEX project_tags_component_idx ON project_tags (type);
CREATE INDEX project_tags_tag_idx ON project_tags (tag);

--------------------------------------------------------------------- | Assets

CREATE TABLE project_images (
	project LABEL NOT NULL,
	type LABEL NOT NULL,
	date_added DATE NOT NULL,
	filename TEXT NOT NULL,
	width INT NOT NULL,
	height INT NOT NULL,
	featured BOOL NOT NULL DEFAULT false,

	PRIMARY KEY (project, filename),
	FOREIGN KEY (project, type, date_added) REFERENCES project_components (project, type, date_added) ON UPDATE CASCADE
);
CREATE UNIQUE INDEX project_images_featured_idx ON project_images (project, type, date_added, featured) WHERE featured = true;

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Helper Functions
\*----------------------------------------------------------------------------------------------------*/

CREATE OR REPLACE FUNCTION add_component(info PROJECT_COMPONENTs, tags TEXT[]) RETURNS VOID AS $$
BEGIN
	-- main data
	INSERT INTO portfolio.project_components
		(project, type, date_added, description, public, archived)
	VALUES
		(info.project, info.type, info.date_added, info.description, info.public, info.archived);

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

CREATE OR REPLACE FUNCTION edit_component(info PROJECT_COMPONENTs, tags TEXT[]) RETURNS VOID AS $$
BEGIN
	-- main data
	UPDATE portfolio.project_components
	SET
		description = info.description,
		public = info.public,
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

CREATE OR REPLACE FUNCTION update_images(info COMPONENT_IDENTITY, _featured TEXT, _delete TEXT[]) RETURNS VOID AS $$
BEGIN
	-- unset the current featured image
	UPDATE portfolio.project_images
	SET
		featured = false
	WHERE
		project = info.project
		AND type = info.type
		AND date_added = info.date_added;

	-- set the new featured image
	UPDATE portfolio.project_images
	SET
		featured = true
	WHERE
		project = info.project
		AND type = info.type
		AND date_added = info.date_added
		AND filename = _featured;

	-- delete files
	DELETE FROM portfolio.project_images
	WHERE
		project = info.project
		AND filename = any(_delete);
END;
$$ LANGUAGE plpgsql VOLATILE;
