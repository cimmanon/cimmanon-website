UPDATE portfolio.project_components_with_tags
SET
	project = ?
	, type = ?
	, date_added = ?
	, description = ?
	, public = ?
	, featured = ?
	, archived = ?
	, tags = ?
WHERE
	(project, type, date_added) = (?, ?, ?)
