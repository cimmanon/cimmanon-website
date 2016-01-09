SELECT
	component,
	description,
	date_added,
	public,
	archived,
	array_agg(tag :: TEXT) AS tags
FROM
	portfolio.project_components
	JOIN portfolio.project_tags USING (project, component, date_added)
WHERE
	project = ?
	AND component = ?
	AND date_added = ?
GROUP BY
	project,
	component,
	date_added
