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
		SELECT DISTINCT ON (project, component)
			MAX(project_components.date_added) OVER (PARTITION BY project) AS last_update,

			project_components.*,

			array_agg(tag :: TEXT) AS tags
		FROM
			portfolio.project_components
			JOIN portfolio.project_tags USING (project, component, date_added)
		WHERE
			project_components.public = true
		GROUP BY
			project, component, date_added
		ORDER BY
			project,
			component,
			date_added DESC) AS components USING (project)
	LEFT JOIN portfolio.project_images USING (project, component, date_added)
WHERE
	projects.featured = true
	AND (project_images.featured = true OR project_images.featured IS NULL)
ORDER BY
	last_update DESC,
	component
;
