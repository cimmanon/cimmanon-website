UPDATE portfolio.project_type_tags
SET
	type = new.new_type
	, tag = new.new_name
	, category = new.category
FROM
	(VALUES (?, ?, ?, ?, ?)) AS new (new_type, new_name, category, old_type, old_name)
WHERE
	(type, tag) = (new.old_type, new.old_name)
