UPDATE portfolio.tag_categories
SET
	category = new.new_name
FROM
	(VALUES (?, ?)) AS new (new_name, old_name)
WHERE
	category = new.old_name
