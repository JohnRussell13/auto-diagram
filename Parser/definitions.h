#ifndef _DEFINITIONS_H
#define _DEFINITIONS_H

/* LENGTH OF NAME IN SYMBOL TABLE */
#define NAME_LENGTH 64
/* MAX NUMBER OF PARAMETERS */
#define MAX_DIM 8
/* LOOP DEPTH */
#define LOOP_DEPTH 256

/* SYMBOLS TABLE KINDS */
enum kinds { NO_KIND, LIT, FUN, VAR, PAR };

/* DATA TYPES */
enum types { NO_TYPE, INT, UINT, VOID, HEX_NUMBER};
/* ARTIHMETIC AND LOGICAL OPERATORS */
enum ops { PLUS, MINUS, STAR, DIV, MOD, SR, SL, AMP, BOR, BXOR, AND, OR };
/* RELATION OPERATORS */
enum relops { LT, LEQ, GT, GEQ, EQ, NEQ };
/* ITERATION OPERATORS */
enum itrops { INC, DEC };

#endif