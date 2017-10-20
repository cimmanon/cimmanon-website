WITH
	components AS (
		SELECT
			MAX(date_added) OVER (PARTITION BY project) AS last_update,
			project_components.*,
			array_agg(tag :: TEXT) AS tags,
			row_number() OVER (PARTITION BY project, type ORDER BY date_added DESC) AS position
		FROM
			portfolio.project_components
			JOIN portfolio.project_tags USING (project, type, date_added)
		WHERE
			lower(type) = ?
			AND project_components.public = true
		GROUP BY
			project,
			type,
			date_added
		)
SELECT
	-- Project
	project,
	projects.description,
	slug,
	url,
	projects.featured,

	-- Component
	type,
	date_added,
	components.description,
	public,
	components.featured,
	archived,
	tags,

	-- Image
	filename,
	width,
	height,
	images.featured
FROM
	portfolio.projects
	JOIN (
		-- select only the top components of each type for any given project
		(SELECT * FROM components WHERE position = 1)
		UNION ALL
		-- select the top 10 from the remaining components
		(SELECT * FROM components WHERE position > 1 ORDER BY position, last_update DESC, date_added DESC LIMIT 10)
		) AS components USING (project)
	LEFT JOIN (SELECT * FROM portfolio.project_images WHERE featured = true) AS images USING (project, type, date_added)
ORDER BY
	last_update DESC,
	project,
	date_added DESC
