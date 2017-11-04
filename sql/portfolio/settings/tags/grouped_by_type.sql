SELECT
	type
	, array_agg(tag :: text ORDER BY category, tag)
FROM
	portfolio.project_type_tags
GROUP BY
	type
ORDER BY
	type
