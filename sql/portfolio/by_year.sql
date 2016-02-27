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
			MAX(date_added) OVER (PARTITION BY project) AS last_update,
			project_components.*,
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
	LEFT JOIN (SELECT * FROM portfolio.project_images WHERE featured = true) AS images USING (project, type, date_added)
ORDER BY
	last_update DESC,
	project,
	date_added DESC
