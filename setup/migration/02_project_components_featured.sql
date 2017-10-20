BEGIN;

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
