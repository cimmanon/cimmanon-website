SELECT
	type,
	description,
	date_added,
	public,
	archived,
	tags,
	filename,
	width,
	height,
	project_images.featured
FROM
	(
		SELECT
			project,
			type,
			description,
			date_added,
			public,
			archived,
			array_agg(tag :: TEXT) AS tags
		FROM
			portfolio.project_components
			JOIN portfolio.project_tags USING (project, type, date_added)
		WHERE
			project = ?
			AND project_components.public = true
		GROUP BY
			project,
			type,
			date_added) AS components
	LEFT JOIN portfolio.project_images USING (project, type, date_added)
ORDER BY
	date_added DESC
