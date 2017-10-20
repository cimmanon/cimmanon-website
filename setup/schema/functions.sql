BEGIN;

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Images
\*----------------------------------------------------------------------------------------------------*/

CREATE OR REPLACE FUNCTION update_images(info COMPONENT_IDENTITY, _featured TEXT, _delete TEXT[]) RETURNS VOID AS $$
BEGIN
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
