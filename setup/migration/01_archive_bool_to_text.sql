SET search_path TO portfolio,public;

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

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Add trigger to fix local path
\*----------------------------------------------------------------------------------------------------*/

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
		$$ LANGUAGE 'plpgsql';
		COMMENT ON FUNCTION update_local_archive_path() IS 'Updates the slug information in local paths';

		CREATE CONSTRAINT TRIGGER roster_revision_checks AFTER UPDATE ON projects
			FOR EACH ROW EXECUTE PROCEDURE update_local_archive_path();

	ELSE
		RAISE NOTICE 'The project_components.archived is not a BOOL, skipping...';
	END IF;
END;
$DO$;
