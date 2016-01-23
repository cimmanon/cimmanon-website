SELECT
	project,
	projects.description,
	slug,
	url,
	projects.featured,

	type,
	components.description,
	date_added,
	public,
	archived,
	tags,

	filename,
	width,
	height,
	project_images.featured
FROM
	portfolio.projects
	JOIN (
		SELECT
			MAX(date_added) OVER (PARTITION BY project) AS last_update,
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
			extract(year from date_added) = ?
			AND project_components.public = true
		GROUP BY
			project,
			type,
			date_added) AS components USING (project)
	LEFT JOIN portfolio.project_images USING (project, type, date_added)
WHERE
	project_images.featured = true OR project_images.featured IS NULL
ORDER BY
	last_update DESC,
	project,
	date_added DESC
