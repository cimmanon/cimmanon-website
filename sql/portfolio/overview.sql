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
	components.featured,
	archived,
	tags,

	filename,
	width,
	height,
	images.featured
FROM
	portfolio.projects
	JOIN (
		SELECT
			MAX(project_components.date_added) OVER (PARTITION BY project) AS last_update,
			project_components.*,
			array_agg(tag :: TEXT) AS tags
		FROM
			portfolio.project_components
			JOIN portfolio.project_tags USING (project, type, date_added)
		WHERE
			public = true
			AND featured = true
		GROUP BY
			project, type, date_added
		ORDER BY
			project,
			type,
			date_added DESC) AS components USING (project)
	LEFT JOIN (SELECT * FROM portfolio.project_images WHERE featured = true) AS images USING (project, type, date_added)
WHERE
	projects.featured = true
ORDER BY
	last_update DESC,
	date_added DESC
;
