SELECT
	-- Project
	project,
	description,
	slug,
	url,
	featured
FROM
	portfolio.projects
WHERE
	slug = ?
