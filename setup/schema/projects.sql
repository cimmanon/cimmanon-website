
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
	public BOOL NOT NULL DEFAULT true,

	PRIMARY KEY (project),
	UNIQUE (slug)
);

--------------------------------------------------------------------- | Components

CREATE TABLE components (
	component LABEL NOT NULL PRIMARY KEY
);

CREATE TABLE project_components (
	project LABEL NOT NULL,
	component LABEL NOT NULL,
	date_added DATE NOT NULL DEFAULT NOW(),
	description TEXT NOT NULL,
	public BOOL NOT NULL default true,

	PRIMARY KEY (project, component, date_added),
	FOREIGN KEY (project) REFERENCES projects (project) ON UPDATE CASCADE,
	FOREIGN KEY (component) REFERENCES components (component) ON UPDATE CASCADE
);
CREATE INDEX project_components_component_idx ON project_components (component);
CREATE INDEX project_components_year_idx ON project_components (extract(year FROM date_added) DESC);

--------------------------------------------------------------------- | Tags

CREATE TABLE tag_categories (
	category LABEL NOT NULL PRIMARY KEY
);

CREATE TABLE component_tags (
	component LABEL NOT NULL,
	tag LABEL NOT NULL,
	category LABEL NOT NULL,

	PRIMARY KEY (component, tag)
);
CREATE INDEX componen_tags_tag_idx ON component_tags (tag);
CREATE INDEX componen_tags_category_idx ON component_tags (category);

CREATE TABLE project_tags (
	project LABEL NOT NULL,
	component LABEL NOT NULL,
	date_added DATE NOT NULL,
	tag LABEL NOT NULL,

	PRIMARY KEY (project, component, date_added, tag),
	FOREIGN KEY (project, component, date_added) REFERENCES project_components (project, component, date_added) ON UPDATE CASCADE,
	FOREIGN KEY (component, tag) REFERENCES component_tags (component, tag) ON UPDATE CASCADE
);
CREATE INDEX project_tag_component_idx ON project_tags (component);
CREATE INDEX project_tag_tag_idx ON project_tags (tag);

--------------------------------------------------------------------- | Assets

-- should we allow multiple images per project?
CREATE TABLE project_images (
	project LABEL NOT NULL,
	component LABEL NOT NULL,
	date_added DATE NOT NULL,
	filename TEXT NOT NULL,
	width INT NOT NULL,
	height INT NOT NULL,
	featured BOOL NOT NULL DEFAULT TRUE,

	PRIMARY KEY (project, component, date_added, filename),
	FOREIGN KEY (project, component, date_added) REFERENCES project_components (project, component, date_added) ON UPDATE CASCADE
);
CREATE UNIQUE INDEX project_images_featured_idx ON project_images (project, component, date_added, featured) WHERE featured = true;
