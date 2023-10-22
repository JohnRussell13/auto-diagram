#include "symtab.h"

void init_symtab(SYMBOL_ENTRY **head){
	*head = NULL;
}

int insert_symbol(SYMBOL_ENTRY **head, char *name, unsigned kind, unsigned type){
	int i;
	int j;
	SYMBOL_ENTRY **temp;
	temp = head;
    SYMBOL_ENTRY *new = (SYMBOL_ENTRY *)malloc(sizeof(SYMBOL_ENTRY));
    if (new == NULL) {
        printf("Not enough RAM!\n");
        exit(1);
    }

	strcpy(new->name, name);
	new->kind = kind;
	new->type = type;
	new->next = NULL;
	for(i = 0; i < MAX_DIM; i++){
		new->dimension[i] = 0;
	}

	new->pointer = 0;

	i = 0;
	while(*temp != NULL){
		temp = &((*temp)->next);
		i++;
	}
	*temp = new;
	
	return i;
}

int lookup_symbol_func(SYMBOL_ENTRY **head, char *name, unsigned func){
	int i;
	SYMBOL_ENTRY *temp;
	temp = *head;
	for(i = 0; i < func; i++){
		if(temp == NULL){
			return -1;
		}
		temp = temp->next;
	}
	while(1){
		if(temp == NULL){
			return -1;
		}
		if(strcmp(temp->name, name) == 0){
			return i;
		}
		temp = temp->next;
		i++;
	}

	return i;
}

int lookup_symbol(SYMBOL_ENTRY **head, char *name){
	int i = 0;
	SYMBOL_ENTRY *temp;
	temp = *head;
	while(1){
		if(temp == NULL){
			return -1;
		}
		if(strcmp(temp->name, name) == 0){
			return i;
		}
		temp = temp->next;
		i++;
	}

	return i;
}

int memory_map(SYMBOL_ENTRY **head, char *name, unsigned func){
	int i, j, k;
	SYMBOL_ENTRY *temp;
	temp = *head;
	for(i = 0; i < func; i++){
		temp = temp->next;
	}
	i = 1;
	while(1){
		if(temp == NULL){
			return -1;
		}
		if(strcmp(temp->name, name) == 0){
			return 4*i;
		}
		if(temp->kind == VAR || temp->kind == PAR){
			if(temp->dimension[0] != 0){
				k = 1;
				for(j = 0; j < MAX_DIM && temp->dimension[j] != 0; j++){
					k *= temp->dimension[j];
				}
				i += k;
			}
			else{
				i++;
			}
		}
		temp = temp->next;
	}

	return 4*i;
}

int function_map(SYMBOL_ENTRY **head, unsigned ind){
	int i, j, k;
	SYMBOL_ENTRY *temp;
	temp = *head;

	for(i = 0; i < ind; i++){
		if(temp == NULL){
			return -1;
		}
		temp = temp->next;
	}
	if(temp == NULL){
		return -1;
	}
	
	temp = temp->next;
	if(temp == NULL){
		i = 2;
		return 4*i;
	}

	i = 2;
	while(temp->kind != FUN){

		if(temp->kind == VAR || temp->kind == PAR){
			if(temp->dimension[0] != 0){
				k = 1;
				for(j = 0; j < MAX_DIM && temp->dimension[j] != 0; j++){
					k *= temp->dimension[j];
				}
				i += k;
			}
			else{
				i++;
			}
		}

		temp = temp->next;
		if(temp == NULL){
			return 4*i;
		}
	}

	return 4*i;
}

/* WHEN IS THIS NEEDED??? */
void set_name(SYMBOL_ENTRY **head, int index, char *name){
	int i;
	SYMBOL_ENTRY *temp;
	temp = *head;

	for(i = 0; i < index; i++){
		temp = temp->next;
	}
	strcpy(temp->name, name);
}
char* get_name(SYMBOL_ENTRY **head, int index){
	int i;
	SYMBOL_ENTRY *temp;
	temp = *head;

	for(i = 0; i < index; i++){
		temp = temp->next;
	}
	return temp->name;
}
void set_kind(SYMBOL_ENTRY **head, int index, unsigned kind){
	int i;
	SYMBOL_ENTRY *temp;
	temp = *head;

	for(i = 0; i < index; i++){
		temp = temp->next;
	}
	temp->kind = kind;
}
unsigned get_kind(SYMBOL_ENTRY **head, int index){
	int i;
	SYMBOL_ENTRY *temp;
	temp = *head;

	for(i = 0; i < index; i++){
		temp = temp->next;
	}
	return temp->kind;
}
void set_type(SYMBOL_ENTRY **head, int index, unsigned type){
	int i;
	SYMBOL_ENTRY *temp;
	temp = *head;

	for(i = 0; i < index; i++){
		temp = temp->next;
	}
	temp->type = type;
}
unsigned get_type(SYMBOL_ENTRY **head, int index){
	int i;
	SYMBOL_ENTRY *temp;
	temp = *head;

	for(i = 0; i < index; i++){
		temp = temp->next;
	}
	return temp->type;
}
void set_dimension(SYMBOL_ENTRY **head, int index, unsigned dimension[MAX_DIM], unsigned size){
	int i;
	SYMBOL_ENTRY *temp;
	temp = *head;

	for(i = 0; i < index; i++){
		temp = temp->next;
	}
	for(i = 0; i < MAX_DIM && i < size; i++){
		temp->dimension[i] = dimension[i];
	}
}
unsigned *get_dimension(SYMBOL_ENTRY **head, int index){
	int i;
	SYMBOL_ENTRY *temp;
	temp = *head;

	for(i = 0; i < index; i++){
		temp = temp->next;
	}
	return temp->dimension;
}
void set_pointer(SYMBOL_ENTRY **head, int index){
	int i;
	SYMBOL_ENTRY *temp;
	temp = *head;

	for(i = 0; i < index; i++){
		temp = temp->next;
	}
	temp->pointer++;
}
unsigned get_pointer(SYMBOL_ENTRY **head, int index){
	int i;
	SYMBOL_ENTRY *temp;
	temp = *head;

	for(i = 0; i < index; i++){
		temp = temp->next;
	}
	return temp->pointer;
}

unsigned get_func(SYMBOL_ENTRY **head){
	int i = 0;
	int j = -1;
	SYMBOL_ENTRY *temp;
	temp = *head;

	while(temp != NULL){
		if(temp->kind == FUN){
			j = i;
		}
		temp = temp->next;
		i++;
	}
	return j;
}

void clear_symbols(SYMBOL_ENTRY **head, unsigned begin_index){
	int i;
	SYMBOL_ENTRY **temp;
	temp = head;

	for(i = 0; i < begin_index; i++){
		temp = &((*temp)->next);
	}
	
	destroy_list(temp);
}

void print_symtab(SYMBOL_ENTRY **head){
	int i = 0;
	int j;
	SYMBOL_ENTRY *temp;
	temp = *head;
	while(temp != NULL){
		printf("INDEX: %d;\tNAME: %s;\tKIND: %d;\t", i, temp->name, temp->kind);
		printf("TYPE %d;\t POINTER: %d\t", temp->type, temp->pointer);
		for(j = 0; j < MAX_DIM; j++){
			if(temp->dimension[j] == 0){
				printf("\n");
				break;
			}
			printf("DIM %d: %d;\t", j, temp->dimension[j]);
		}

		temp = temp->next;
		i++;
	}
}

/* DON"T KNOW REALLY HOW TO AVOID RECURSION IN THIS ONE... MAYBE TWO WAY LIST? IS IT WORTH IT? */
void destroy_list(SYMBOL_ENTRY **head){
    if(*head != NULL) {
        destroy_list(&((*head)->next));
        free(*head);
        *head = NULL;
    }
}