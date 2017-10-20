BEGIN;

\echo 'Checking the type of project_components.archived...'

-- only make changes here if project_components.archived is a boolean.
DO LANGUAGE plpgsql $DO$
BEGIN
	IF EXISTS (
		SELECT 1 FROM information_schema.columns
		WHERE
			table_name = 'project_components'
			AND column_name = 'archived'
			AND data_type = 'bool')
	THEN

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Change column type and information
\*----------------------------------------------------------------------------------------------------*/

		ALTER table project_components RENAME archived TO old_archived;
		ALTER table project_components ADD archived TEXT;

		UPDATE project_components AS c
		SET archived = '/archives/' || p.slug || '/' || c.date_added || '/'
		FROM projects AS p
		WHERE p.project = c.project AND c.old_archived = true
		RETURNING c.project, c.date_added, c.archived;

		ALTER table project_components DROP old_archived;

	ELSE
		RAISE NOTICE 'The project_components.archived is not a BOOL, skipping...';
	END IF;
END;
$DO$;
