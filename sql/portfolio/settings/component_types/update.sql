UPDATE portfolio.project_types
SET
	type = new.new_name
FROM
	(VALUES (?, ?)) AS new (new_name, old_name)
WHERE
	type = new.old_name
