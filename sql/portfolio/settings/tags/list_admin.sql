SELECT
	type
	, tag
	, category
FROM
	portfolio.project_types
	LEFT JOIN portfolio.project_type_tags USING (type)
ORDER BY
	type
	, category
	, tag
