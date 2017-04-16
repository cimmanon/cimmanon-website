SELECT
	type,
	description,
	date_added,
	public,
	featured,
	archived,
	array[] :: text[] AS tags
FROM
	portfolio.project_components
WHERE
	project = ?
ORDER BY date_added
