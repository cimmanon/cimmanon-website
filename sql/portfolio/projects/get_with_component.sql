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
	project_components.description,
	public,
	project_components.featured,
	archived,
	array[] :: TEXT[]
--	COALESCE(array_agg(tag :: TEXT) FILTER (WHERE tag IS NOT NULL), array[] :: TEXT[]) AS tags
FROM
	portfolio.projects
	JOIN portfolio.project_components USING (project)
	LEFT JOIN portfolio.project_tags USING (project, type, date_added)
WHERE
	slug = ?
	AND type = ?
	AND date_added = ?
/*
GROUP BY
	project,
	type,
	date_added
*/