
INSERT INTO components
	(component)
VALUES
	('Design'),
	('Development')
;

---------------------------------------------------------------------

INSERT INTO tag_categories
	(category)
VALUES
	('Layout'),
	('Color'),
	('Medium'),
	('Language'),
	('Application Type'),
	('Database')
;

INSERT INTO component_tags
	(component, category, tag)
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
	('Development', 'Application Type', 'cli'),
	('Development', 'Application Type', 'gui'),
	('Development', 'Application Type', 'web'),
	('Development', 'Application Type', 'library'),
	('Development', 'Database', 'mysql'),
	('Development', 'Database', 'postgresql'),
	('Development', 'Database', 'mssql')
;
