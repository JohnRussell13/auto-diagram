#ifndef _SYMTAB_H
#define _SYMTAB_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "definitions.h"

/* STRUCTURE OF THE SYMBOL TABLE ELEMENTS */
typedef struct sym_entry{
	char name[NAME_LENGTH]; //name of symbol
	unsigned kind; //type of symbol
	unsigned type; //type of value of symbol
	unsigned pointer; //type of value of symbol
	unsigned dimension[MAX_DIM];
	struct sym_entry *next;
} SYMBOL_ENTRY;

/* INT TABLE */
void init_symtab(SYMBOL_ENTRY **head);
/* INSERT A SYMBOL INTO THE TABLE */
int insert_symbol(SYMBOL_ENTRY **head, char *name, unsigned kind, unsigned type);

/* FUNCTION FOR SEARCHING FOR SYMBOL IN SYMBOL TABLE */
int lookup_symbol(SYMBOL_ENTRY **head, char *name);
int lookup_symbol_func(SYMBOL_ENTRY **head, char *name, unsigned func);

/* MEMORY MAPS */
int memory_map(SYMBOL_ENTRY **head, char *name, unsigned func);
int function_map(SYMBOL_ENTRY **head, unsigned ind);

/* METHODS FOR UPDATING ELEMENTS IN SYMBOL TABLE */
void set_name(SYMBOL_ENTRY **head, int index, char *name);
char* get_name(SYMBOL_ENTRY **head, int index);
void set_kind(SYMBOL_ENTRY **head, int index, unsigned kind);
unsigned get_kind(SYMBOL_ENTRY **head, int index);
void set_type(SYMBOL_ENTRY **head, int index, unsigned type);
unsigned get_type(SYMBOL_ENTRY **head, int index);
void set_dimension(SYMBOL_ENTRY **head, int index, unsigned dimension[MAX_DIM], unsigned size);
unsigned *get_dimension(SYMBOL_ENTRY **head, int index);
void set_pointer(SYMBOL_ENTRY **head, int index);
unsigned get_pointer(SYMBOL_ENTRY **head, int index);

/* INDEX OF THE CURRENT FUNCTION */
unsigned get_func(SYMBOL_ENTRY **head);

/* REMOVE SUBTABLE -- DELETES EVERYTHING AFTER begin_index */
void clear_symbols(SYMBOL_ENTRY **head, unsigned begin_index);

/* DISPLAY SYMBOL TABLE */
void print_symtab(SYMBOL_ENTRY **head);

/* DELETE TABLE -- DELETES EVERYTHING BELOVE head */
void destroy_list(SYMBOL_ENTRY **head);

#endif
