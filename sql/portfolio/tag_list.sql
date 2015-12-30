SELECT
	category,
	array_agg(tag :: TEXT ORDER BY tag) AS tags
FROM
	portfolio.component_tags
GROUP BY
	category
ORDER BY
	category
