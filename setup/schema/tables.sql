/*----------------------------------------------------------------------------------------------------*\
                                                                      | Types
\*----------------------------------------------------------------------------------------------------*/

--------------------------------------------------------------------- | Domains

CREATE DOMAIN label AS TEXT
	CONSTRAINT label_not_empty_ck CHECK (VALUE != '')
	CONSTRAINT label_max_length_ck CHECK (char_length(VALUE) <= 150)
;

CREATE DOMAIN slug as TEXT
--	CONSTRAINT slug_not_empty_ck CHECK (VALUE != '')
	CONSTRAINT slug_valid_characters_ck CHECK (value ~* '^[a-zA-Z0-9\-\._]+$');

--------------------------------------------------------------------- | Composite Types

CREATE TYPE component_identity AS (project LABEL, type LABEL, date_added DATE);

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
	CONSTRAINT projects_slug_key UNIQUE (slug)
);

--------------------------------------------------------------------- | Project types

CREATE TABLE project_types (
	type LABEL NOT NULL,

	PRIMARY KEY (type)
);

CREATE TABLE project_components (
	project LABEL NOT NULL,
	type LABEL NOT NULL,
	date_added DATE NOT NULL DEFAULT NOW(),
	description TEXT NOT NULL,
	public BOOL NOT NULL default true,
	featured BOOL NOT NULL default false,
	archived TEXT,

	PRIMARY KEY (project, type, date_added),
	CONSTRAINT project_components_project_fkey FOREIGN KEY (project)
		REFERENCES projects (project)
		ON UPDATE CASCADE ON DELETE CASCADE,
	CONSTRAINT project_components_type_fkey FOREIGN KEY (type)
		REFERENCES project_types (type)
		ON UPDATE CASCADE
);
CREATE INDEX project_components_component_idx ON project_components (type);
CREATE INDEX project_components_year_idx ON project_components (extract(year FROM date_added) DESC);

--------------------------------------------------------------------- | Tags

CREATE TABLE tag_categories (
	category LABEL NOT NULL,

	PRIMARY KEY (category)
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
	CONSTRAINT project_tags_project_fkey FOREIGN KEY (project, type, date_added)
		REFERENCES project_components (project, type, date_added)
		ON UPDATE CASCADE ON DELETE CASCADE,
	CONSTRAINT project_tags_type_fkey FOREIGN KEY (type, tag)
		REFERENCES project_type_tags (type, tag)
		ON UPDATE CASCADE
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
	CONSTRAINT project_images_project_fkey FOREIGN KEY (project, type, date_added)
		REFERENCES project_components (project, type, date_added)
		ON UPDATE CASCADE ON DELETE CASCADE
);
CREATE UNIQUE INDEX project_images_featured_idx ON project_images (project, type, date_added, featured) WHERE featured = true;
