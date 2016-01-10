INSERT INTO portfolio.project_images
	(project, component, date_added, filename, width, height)
SELECT
	project,
	?,
	?,
	?,
	?,
	?
FROM
	portfolio.projects
WHERE
	slug = ?;
