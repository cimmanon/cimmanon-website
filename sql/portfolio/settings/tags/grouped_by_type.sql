SELECT
	type
	, coalesce(array_agg(tag :: TEXT ORDER BY category, tag) FILTER (WHERE tag IS NOT NULL), array[] :: TEXT[])
FROM
	portfolio.project_types
	LEFT JOIN portfolio.project_type_tags USING (type)
GROUP BY
	type
ORDER BY
	type
