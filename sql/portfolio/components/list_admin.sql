SELECT DISTINCT ON (date_added, type)
	-- Component
	type,
	date_added,
	description,
	public,
	project_components.featured,
	archived,
	array[] :: text[] AS tags,

	-- Image
	filename,
	width,
	height,
	project_images.featured
FROM
	portfolio.project_components
	LEFT JOIN portfolio.project_images USING (project, type, date_added)
WHERE
	project = ?
ORDER BY
	date_added,
	type,
	project_images.featured DESC
