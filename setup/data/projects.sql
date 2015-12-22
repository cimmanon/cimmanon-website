
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
	('Design', 'Color', 'blue'),
	('Design', 'Color', 'green'),
	('Design', 'Color', 'pink'),
	('Design', 'Color', 'neutral'),
	('Design', 'Color', 'brown'),
	('Design', 'Color', 'orange'),
	('Design', 'Color', 'white'),

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
