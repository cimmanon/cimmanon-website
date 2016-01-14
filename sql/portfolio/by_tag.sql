SELECT
	project,
	projects.description,
	slug,
	url,
	projects.featured,

	component,
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
			component,
			description,
			date_added,
			public,
			archived,
			array_agg(tag :: TEXT) AS tags
		FROM
			portfolio.project_components
			JOIN portfolio.project_tags USING (project, component, date_added)
			-- subquery here to avoid sequential scan
			JOIN (
				SELECT
					project,
					component,
					date_added
				FROM
					portfolio.project_tags
				WHERE
					tag = ?
				) AS has_tag USING (project, component, date_added)
		WHERE
			project_components.public = true
		GROUP BY
			project,
			component,
			date_added) AS components USING (project)
	LEFT JOIN portfolio.project_images USING (project, component, date_added)
WHERE
	project_images.featured = true OR project_images.featured IS NULL
ORDER BY
	last_update DESC,
	project,
	date_added DESC
