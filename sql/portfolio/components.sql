SELECT
	component,
	description,
	date_added,
	archived,
	tags,
	filename,
	width,
	height
FROM
	(
		SELECT
			project,
			component,
			description,
			date_added,
			archived,
			array_agg(tag :: TEXT) AS tags
		FROM
			portfolio.project_components
			JOIN portfolio.project_tags USING (project, component, date_added)
		WHERE
			project = ?
			AND project_components.public = true
		GROUP BY
			project,
			component,
			date_added) AS components
	LEFT JOIN portfolio.project_images USING (project, component, date_added)
ORDER BY
	date_added DESC
