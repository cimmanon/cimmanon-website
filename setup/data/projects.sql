BEGIN;

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Project Types
\*----------------------------------------------------------------------------------------------------*/

INSERT INTO project_types
	(type)
VALUES
	('Design'),
	('Development')
EXCEPT
SELECT type FROM project_types
;

/*----------------------------------------------------------------------------------------------------*\
                                                                      | Tags
\*----------------------------------------------------------------------------------------------------*/

INSERT INTO tag_categories
	(category)
VALUES
	('Layout'),
	('Color'),
	('Medium'),
	('Language'),
	('Application Type'),
	('Database')
EXCEPT
SELECT category FROM tag_categories
;

INSERT INTO project_type_tags
	(type, category, tag)
VALUES
	-- design
	('Design', 'Layout', 'fixed'),
	('Design', 'Layout', 'elastic'),
	('Design', 'Layout', 'liquid'),
	('Design', 'Layout', 'responsive'),
	('Design', 'Medium', 'web'),
	('Design', 'Medium', 'logo'), -- not sure about this
	('Design', 'Color', 'purple'),
	('Design', 'Color', 'blue'),
	('Design', 'Color', 'green'),
	('Design', 'Color', 'yellow'),
	('Design', 'Color', 'orange'),
	('Design', 'Color', 'red'),
	('Design', 'Color', 'pink'),
	('Design', 'Color', 'natural'),
	('Design', 'Color', 'brown'),
	('Design', 'Color', 'white'),
	('Design', 'Color', 'grey'),
	('Design', 'Color', 'black'),

	-- development
	('Development', 'Language', 'haskell'),
	('Development', 'Language', 'php'),
	('Development', 'Language', 'pike'),
	('Development', 'Language', 'javascript'),
	('Development', 'Language', 'sass'),
	('Development', 'Language', 'visual-basic'),
	('Development', 'Application Type', 'cli'),
	('Development', 'Application Type', 'gui'),
	('Development', 'Application Type', 'web'),
	('Development', 'Application Type', 'library'),
	('Development', 'Database', 'mysql'),
	('Development', 'Database', 'postgresql'),
	('Development', 'Database', 'mssql')
EXCEPT
SELECT type, category, tag FROM project_type_tags
;
