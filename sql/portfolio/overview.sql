SELECT
	name,
	overview,
	slug,
	url,
	public,

	component,
	description,
	date_added,
	tags
FROM
	(SELECT DISTINCT ON (projects.name, project_components.component)
		MAX(project_components.date_added) OVER (PARTITION BY projects.name) AS last_update,

		projects.name,
		projects.description AS overview,
		projects.slug,
		projects.url,
		projects.public,

		project_components.component,
		project_components.description,
		project_components.date_added,

		array_agg(tag :: TEXT) AS tags
	FROM
		portfolio.projects
		JOIN portfolio.project_components on project_components.project = projects.name
		JOIN portfolio.project_tags USING (project, component, date_added)
	WHERE
		projects.public = true
		AND project_components.public = true
	GROUP BY
		projects.name, project, component, date_added
	ORDER BY
		projects.name,
		project_components.component,
		project_components.date_added DESC) AS x
ORDER BY
	last_update DESC,
	component
;
