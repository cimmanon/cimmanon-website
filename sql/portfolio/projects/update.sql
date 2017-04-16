UPDATE portfolio.projects
SET
	project = ?,
	description = ?,
	slug = ?,
	url = ?,
	featured = ?
WHERE
	project = ?
