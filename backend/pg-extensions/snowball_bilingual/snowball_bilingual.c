
/*-------------------------------------------------------------------------
 *
 * snowball_bilingual.c
 *	  Snowball dictionary as extension
 *
 * Copyright (c) 2007-2019, PostgreSQL Global Development Group
 *
 * 2019 Modified for bilinguality by MediaTUM project, Thomas Weiser
 *
 *-------------------------------------------------------------------------
 */
#include "postgres.h"

#include "commands/defrem.h"
#include "tsearch/ts_locale.h"
#include "tsearch/ts_utils.h"

/* Now we can include the original Snowball header.h */
#include "snowball/libstemmer/header.h"
#include "libstemmer/stem_UTF_8_english.h"
#include "libstemmer/stem_UTF_8_german.h"

PG_MODULE_MAGIC;

PG_FUNCTION_INFO_V1(dsnowball_bilingual_init);
PG_FUNCTION_INFO_V1(dsnowball_bilingual_lexize);

/* List of supported modules */
typedef struct stemmer_module
{
	const char *name;
	pg_enc enc;
	struct SN_env *(*create)(void);
	void (*close)(struct SN_env *);
	int (*stem)(struct SN_env *);
} stemmer_module;

static const stemmer_module stemmer_modules[] =
		{
				// Stemmers list from Snowball distribution
				{"english", PG_UTF8, english_UTF_8_create_env, english_UTF_8_close_env, english_UTF_8_stem},
				{"german", PG_UTF8, german_UTF_8_create_env, german_UTF_8_close_env, german_UTF_8_stem},

				{NULL, 0, NULL, NULL, NULL} /* list end marker */
};

typedef struct DictSnowballLanguage
{
	struct SN_env *z;
	bool needrecode; /* needs recoding before/after call stem */
	int (*stem)(struct SN_env *z);
	StopList stoplist;
} DictSnowballLanguage;

typedef struct DictSnowballBilingual
{
	DictSnowballLanguage languages[2];

	/*
	 * snowball saves alloced memory between calls, so we should run it in our
	 * private memory context. Note, init function is executed in long lived
	 * context, so we just remember CurrentMemoryContext
	 */
	MemoryContext dictCtx;
} DictSnowballBilingual;

static void
locate_stem_module(DictSnowballLanguage *dl, char *lang)
{
	const stemmer_module *m;

	readstoplist(lang, &dl->stoplist, lowerstr);

	/*
	 * First, try to find exact match of stemmer module. Stemmer with
	 * PG_SQL_ASCII encoding is treated as working with any server encoding
	 */
	for (m = stemmer_modules; m->name; m++)
	{
		if ((m->enc == PG_SQL_ASCII || m->enc == GetDatabaseEncoding()) &&
				pg_strcasecmp(m->name, lang) == 0)
		{
			dl->stem = m->stem;
			dl->z = m->create();
			dl->needrecode = false;
			return;
		}
	}

	/*
	 * Second, try to find stemmer for needed language for UTF8 encoding.
	 */
	for (m = stemmer_modules; m->name; m++)
	{
		if (m->enc == PG_UTF8 && pg_strcasecmp(m->name, lang) == 0)
		{
			dl->stem = m->stem;
			dl->z = m->create();
			dl->needrecode = true;
			return;
		}
	}

	ereport(ERROR,
					(errcode(ERRCODE_UNDEFINED_OBJECT),
					 errmsg("no Snowball stemmer available for language \"%s\" and encoding \"%s\"",
									lang, GetDatabaseEncodingName())));
}

Datum
		dsnowball_bilingual_init(PG_FUNCTION_ARGS)
{
	List *dictoptions = (List *)PG_GETARG_POINTER(0);
	DictSnowballBilingual *db;
	ListCell *l;

	// elog(WARNING, "sizeof(DictSnowballLanguage) = %lu", sizeof(DictSnowballLanguage));
	// elog(WARNING, "sizeof(DictSnowballBilingual) = %lu", sizeof(DictSnowballBilingual));

	db = (DictSnowballBilingual *)palloc0(sizeof(DictSnowballBilingual));

	foreach (l, dictoptions)
	{
		DefElem *defel = (DefElem *)lfirst(l);

		if (pg_strcasecmp("StopWords", defel->defname) == 0)
		{
			ereport(ERROR,
							(errcode(ERRCODE_INVALID_PARAMETER_VALUE),
							 errmsg("unexpected StopWords parameter. This bilingual Snowball template is fixed to English and German.")));
		}
		else if (pg_strcasecmp("Language", defel->defname) == 0)
		{
			ereport(ERROR,
							(errcode(ERRCODE_INVALID_PARAMETER_VALUE),
							 errmsg("unexpected Language parameter. This bilingual Snowball template is fixed to English and German.")));
		}
		else
		{
			ereport(ERROR,
							(errcode(ERRCODE_INVALID_PARAMETER_VALUE),
							 errmsg("unrecognized Snowball parameter: \"%s\"",
											defel->defname)));
		}
	}

	locate_stem_module(&(db->languages[0]), "english");
	locate_stem_module(&(db->languages[1]), "german");

	db->dictCtx = CurrentMemoryContext;

	PG_RETURN_POINTER(db);
}

static void
lexize_language(TSLexeme *res, char *txt, DictSnowballLanguage *dl, MemoryContext dictCtx)
{
	MemoryContext saveCtx;

	/*
	 * recode to utf8 if stemmer is utf8 and doesn't match server encoding
	 */
	if (dl->needrecode)
	{
		char *recoded;

		recoded = pg_server_to_any(txt, strlen(txt), PG_UTF8);
		if (recoded != txt)
		{
			pfree(txt);
			txt = recoded;
		}
	}

	/* see comment about db->dictCtx */
	saveCtx = MemoryContextSwitchTo(dictCtx);
	SN_set_current(dl->z, strlen(txt), (symbol *)txt);
	dl->stem(dl->z);
	MemoryContextSwitchTo(saveCtx);

	if (dl->z->p && dl->z->l)
	{
		txt = repalloc(txt, dl->z->l + 1);
		memcpy(txt, dl->z->p, dl->z->l);
		txt[dl->z->l] = '\0';
	}

	/* back recode if needed */
	if (dl->needrecode)
	{
		char *recoded;

		recoded = pg_any_to_server(txt, strlen(txt), PG_UTF8);
		if (recoded != txt)
		{
			pfree(txt);
			txt = recoded;
		}
	}

	res->lexeme = txt;
}

Datum
		dsnowball_bilingual_lexize(PG_FUNCTION_ARGS)
{
	DictSnowballBilingual *db = (DictSnowballBilingual *)PG_GETARG_POINTER(0);
	char *in = (char *)PG_GETARG_POINTER(1);
	int32 len = PG_GETARG_INT32(2);
	TSLexeme *res = palloc0(sizeof(TSLexeme) * 3);

	char *txt = lowerstr_with_len(in, len);

	if (*txt == '\0')
	{
		pfree(txt);
	}
	else
	{
		int lexemeCount = 0;

		if (searchstoplist(&(db->languages[0].stoplist), txt))
		{
			pfree(txt);
		}
		else
		{
			lexize_language(res + (lexemeCount++), txt, &(db->languages[0]), db->dictCtx);
		}

		// TODO: Don't copy the string again. Needs some memory management refactoring.
		txt = lowerstr_with_len(in, len);

		if (searchstoplist(&(db->languages[1].stoplist), txt))
		{
			pfree(txt);
		}
		else
		{
			lexize_language(res + (lexemeCount++), txt, &(db->languages[1]), db->dictCtx);
		}

		if (lexemeCount == 2)
		{
			// Mark the second lexeme a variant of the first lexeme.
			// This is relevant for queries to combine the lexemes by `|` instead of `&`
			// (i.e. disjuction instead of conjunction).
			res[1].nvariant = 1;

			// If the lexemes are equal then just keep one version
			if (strcmp(res[0].lexeme, res[1].lexeme) == 0)
			{
				memset(res + 1, 0, sizeof(TSLexeme));
			}
		}
	}

	PG_RETURN_POINTER(res);
}
