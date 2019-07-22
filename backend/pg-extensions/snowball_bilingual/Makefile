
MODULE_big = snowball_bilingual

EXTENSION = snowball_bilingual
PGFILEDESC = "snowball_bilingual - add-on dictionary template using the combined results of two snowball stemmers"

REGRESS = snowball_bilingual
ENCODING = UTF8
OBJS= snowball_bilingual.o libstemmer/api.o libstemmer/utilities.o \
	libstemmer/stem_UTF_8_english.o \
	libstemmer/stem_UTF_8_german.o \
	$(WIN32RES)

SQLSCRIPT= snowball_bilingual--1.0.sql
DATA_built = $(SQLSCRIPT)

PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)

override CPPFLAGS += -I$(CURDIR)/libstemmer $(CPPFLAGS)
VPATH += $(CURDIR)/libstemmer
