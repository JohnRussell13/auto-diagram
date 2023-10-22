#ifndef _STACK_H
#define _STACK_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "definitions.h"

#define STACK_DEPTH 10

/* STACK */
void push(char *reg, int size);
void pop(char *reg, int size);

/* COMPILER STACK */
void c_push(int *stack, int *pos, int val);
void c_pop(int *stack, int *pos, int *val);

#endif
