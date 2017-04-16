SELECT
	DISTINCT extract(year FROM date_added) :: Int
FROM
	portfolio.project_components
ORDER BY
	extract(year FROM date_added) :: Int DESC
