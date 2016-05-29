SET search_path TO portfolio,public;

\echo 'Checking to see if project_components.featured exists...'

-- only make changes here if project_components.featured does not exist.
DO LANGUAGE plpgsql $DO$
BEGIN
	IF NOT EXISTS (
		SELECT 1 FROM information_schema.columns
		WHERE
			table_name = 'project_components'
			AND column_name = 'featured')
	THEN

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
				project_components
				JOIN project_tags USING (project, type, date_added)
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

	ELSE
		RAISE NOTICE 'The project_components.featured column already exists, skipping...';
	END IF;
END;
$DO$;
