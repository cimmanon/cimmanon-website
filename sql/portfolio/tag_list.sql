SELECT
	-- Category
	category,
	array_agg(tag :: TEXT ORDER BY tag) AS tags
FROM
	(SELECT DISTINCT ON (category, tag) * FROM portfolio.project_type_tags ORDER BY category, tag) AS x
GROUP BY
	category
ORDER BY
	category
