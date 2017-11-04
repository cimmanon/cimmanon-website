SELECT
	-- Tag
	type
	, tag
	, category
FROM
	portfolio.project_type_tags
ORDER BY
	type
	, category
	, tag
