DELETE FROM portfolio.project_type_tags
WHERE
	(type, tag) IN (VALUES (?, ?))
