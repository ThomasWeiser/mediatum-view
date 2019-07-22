CREATE EXTENSION snowball_bilingual;

SELECT * FROM ts_debug('english_german','Welt Welten world worlds');

SELECT to_tsvector('english_german', 'Welt Welten world worlds');
SELECT to_tsquery('english_german', 'Welten');

CREATE TABLE table1(name varchar);
INSERT INTO table1 VALUES
	('I''m sorry'),
	('Ich bitte um Entschuldigung'),
	('PostgreSQL the world''s most advanced open source databases'),
	('PostgreSQL das fortschrittlichste Open-Source-Datenbanksystem der Welt'),
	('i me my myself the'),
	('als also am an der die das');
SELECT d.* FROM table1 AS t, LATERAL ts_debug('english_german', t.name) AS d;

CREATE INDEX name_idx ON table1 USING GIN (to_tsvector('english_german', "name"));

SELECT * FROM table1 WHERE to_tsvector('english_german', name)
	@@ to_tsquery('english_german', 'sorry');
SELECT * FROM table1 WHERE to_tsvector('english_german', name)
	@@ to_tsquery('english_german', 'entschuldigen');
SELECT * FROM table1 WHERE to_tsvector('english_german', name)
	@@ to_tsquery('english_german', 'sources');
SELECT * FROM table1 WHERE to_tsvector('english_german', name)
	@@ to_tsquery('english_german', 'Welten');

DROP INDEX name_idx;

CREATE INDEX name_idx ON table1 USING GIST (to_tsvector('english_german', "name"));

SELECT * FROM table1 WHERE to_tsvector('english_german', name)
	@@ to_tsquery('english_german', 'sorry');
SELECT * FROM table1 WHERE to_tsvector('english_german', name)
	@@ to_tsquery('english_german', 'entschuldigen');
SELECT * FROM table1 WHERE to_tsvector('english_german', name)
	@@ to_tsquery('english_german', 'sources');
SELECT * FROM table1 WHERE to_tsvector('english_german', name)
	@@ to_tsquery('english_german', 'Welten');

DROP INDEX name_idx;
