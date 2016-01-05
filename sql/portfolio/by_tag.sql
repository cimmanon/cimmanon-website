SELECT
	project,
	projects.description,
	slug,
	url,
	projects.public,

	component,
	components.description,
	date_added,
	archived,
	tags,

	filename,
	width,
	height
FROM
	portfolio.projects
	JOIN (
		SELECT
			MAX(date_added) OVER (PARTITION BY project) AS last_update,
			project,
			component,
			description,
			date_added,
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
		GROUP BY
			project,
			component,
			date_added) AS components USING (project)
	LEFT JOIN portfolio.project_images USING (project, component, date_added)
ORDER BY
	last_update DESC,
	project,
	date_added DESC
