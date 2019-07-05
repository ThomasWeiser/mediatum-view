-- Language-specific snowball dictionaries
/*
 * Create underlying C functions for Snowball stemmers
 *
 * Copyright (c) 2007-2018, PostgreSQL Global Development Group
 *
 * contrib/snowball_bilingual/snowball_bilingual_func.sql.in
 *
 * This file is combined with multiple instances of snowball_bilingual.sql.in to
 * build snowball_bilingual--1.0.sql.
 *
 * Note: this file is read in single-user -j mode, which means that the
 * command terminator is semicolon-newline-newline; whenever the backend
 * sees that, it stops and executes what it's got.  If you write a lot of
 * statements without empty lines between, they'll all get quoted to you
 * in any error message about one of them, so don't do that.  Also, you
 * cannot write a semicolon immediately followed by an empty line in a
 * string literal (including a function body!) or a multiline comment.
 */

CREATE FUNCTION dsnowball_bilingual_init(INTERNAL)
    RETURNS INTERNAL AS 'MODULE_PATHNAME', 'dsnowball_bilingual_init'
LANGUAGE C STRICT;

CREATE FUNCTION dsnowball_bilingual_lexize(INTERNAL, INTERNAL, INTERNAL, INTERNAL)
    RETURNS INTERNAL AS 'MODULE_PATHNAME', 'dsnowball_bilingual_lexize'
LANGUAGE C STRICT;

CREATE TEXT SEARCH TEMPLATE snowball_bilingual
	(INIT = dsnowball_bilingual_init,
	LEXIZE = dsnowball_bilingual_lexize);

COMMENT ON TEXT SEARCH TEMPLATE snowball_bilingual IS 'bilingual snowball stemmer';
/*
 * bilingual text search configuration for English and German
 *
 * Copyright (c) 2007-2018, PostgreSQL Global Development Group
 *
 * contrib/snowball_bilingual/snowball_bilingual.sql.in
 *
 * Note: this file is read in single-user -j mode, which means that the
 * command terminator is semicolon-newline-newline; whenever the backend
 * sees that, it stops and executes what it's got.  If you write a lot of
 * statements without empty lines between, they'll all get quoted to you
 * in any error message about one of them, so don't do that.  Also, you
 * cannot write a semicolon immediately followed by an empty line in a
 * string literal (including a function body!) or a multiline comment.
 */

CREATE TEXT SEARCH DICTIONARY english_german_stem
	(TEMPLATE = snowball_bilingual );

COMMENT ON TEXT SEARCH DICTIONARY english_german_stem IS 'bilingual snowball stemmer for english and german language';

CREATE TEXT SEARCH CONFIGURATION english_german
	(PARSER = default);

COMMENT ON TEXT SEARCH CONFIGURATION english_german IS 'configuration for bilingual english and german language';

ALTER TEXT SEARCH CONFIGURATION english_german ADD MAPPING
	FOR email, url, url_path, host, file, version,
	    sfloat, float, int, uint,
	    numword, hword_numpart, numhword
	WITH simple;

ALTER TEXT SEARCH CONFIGURATION english_german ADD MAPPING
    FOR asciiword, hword_asciipart, asciihword
	WITH english_german_stem;

ALTER TEXT SEARCH CONFIGURATION english_german ADD MAPPING
    FOR word, hword_part, hword
	WITH english_german_stem;
