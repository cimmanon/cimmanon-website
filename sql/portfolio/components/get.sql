SELECT
	-- Component
	type,
	description,
	date_added,
	public,
	featured,
	archived,
	COALESCE(array_agg(tag :: TEXT) FILTER (WHERE tag IS NOT NULL), array[] :: TEXT[]) AS tags
FROM
	portfolio.project_components
	LEFT JOIN portfolio.project_tags USING (project, type, date_added)
WHERE
	project = ?
	AND type = ?
	AND date_added = ?
GROUP BY
	project,
	type,
	date_added
