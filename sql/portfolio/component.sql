SELECT
	component,
	description,
	date_added,
	public,
	archived,
	COALESCE(array_agg(tag :: TEXT) FILTER (WHERE tag IS NOT NULL), array[] :: TEXT[]) AS tags
FROM
	portfolio.project_components
	LEFT JOIN portfolio.project_tags USING (project, component, date_added)
WHERE
	project = ?
	AND component = ?
	AND date_added = ?
GROUP BY
	project,
	component,
	date_added
