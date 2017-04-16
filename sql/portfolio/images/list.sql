SELECT
	-- Image
	filename,
	width,
	height,
	featured
FROM
	portfolio.project_images
WHERE
	(project, type, date_added) = (?, ?, ?)
ORDER BY
	featured DESC,
	filename
