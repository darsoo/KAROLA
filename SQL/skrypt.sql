create table abstracts(pmid integer,words integer[]);
copy abstracts from 'abstracts.test.tab' DELIMITERS E'\t' ;

create table date(pmid integer,date date);
copy date from 'date.test.tab' DELIMITERS E'\t' ;

create table date_table(id_date integer,date date, new_date integer);
copy date_table from 'date_table.test.tab' DELIMITERS E'\t' ;

create table library(word_id integer, word text);
copy library from 'library.test.tab' DELIMITERS E'\t' ;

