SET search_path TO portfolio,public;

\echo 'Adjusting foreign key cascade settings...'

-- only make changes here if our foreign keys are not set to cascade on delete...
DO LANGUAGE plpgsql $DO$
BEGIN
	IF EXISTS (
		SELECT 1 FROM
			information_schema.table_constraints
			JOIN information_schema.referential_constraints USING (constraint_name)
		WHERE
			constraint_type = 'FOREIGN KEY'
			AND delete_rule != 'CASCADE'
			AND constraint_name IN ('project_components_project_fkey', 'project_tags_project_fkey', 'project_images_project_fkey')
		)
	THEN

		-- project_components
		ALTER TABLE project_components DROP CONSTRAINT project_components_project_fkey;
		ALTER TABLE project_components ADD FOREIGN KEY (project)
			REFERENCES projects(project)
			ON UPDATE CASCADE ON DELETE CASCADE;

		-- project_tags
		ALTER TABLE project_tags DROP CONSTRAINT project_tags_project_fkey;
		ALTER TABLE project_tags ADD FOREIGN KEY (project, type, date_added)
			REFERENCES project_components(project, type, date_added)
			ON UPDATE CASCADE ON DELETE CASCADE;

		-- project_images
		ALTER TABLE project_images DROP CONSTRAINT project_images_project_fkey;
		ALTER TABLE project_images ADD FOREIGN KEY (project, type, date_added)
			REFERENCES project_components(project, type, date_added)
			ON UPDATE CASCADE ON DELETE CASCADE;

	ELSE
		RAISE NOTICE 'The foreign keys are already correctly set to cascade on delete';
	END IF;
END;
$DO$;
