SELECT
	component,
	description,
	date_added,
	array_agg(tag :: TEXT)
FROM
	portfolio.project_components
	JOIN portfolio.project_tags USING (project, component, date_added)
WHERE project = ?
GROUP BY
	project,
	component,
	date_added
ORDER BY
	date_added
