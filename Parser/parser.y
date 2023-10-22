%{
    #include <stdio.h>
    #include "definitions.h"
    #include "symtab.h"
    #include "stack.h"

    /* FLEX/BISON REQUIRED */
    int yyparse(void);
    int yylex(void);
    int yyerror(char *s);
    extern int yylineno;
    int error_count;

    /* USED VARIABLES */
    int lab_cnt = 0; // LABEL COUNTER
    int for_depth = 0; // FOR LABEL
    int for_layer[LOOP_DEPTH];  // FOR LABEL
    int while_depth = 0; // WHILE LABEL
    int while_layer[LOOP_DEPTH]; // WHILE LABEL
    int if_depth = 0; // WHILE LABEL
    int if_layer[LOOP_DEPTH]; // WHILE LABEL
    int args = 1; // FUNCTION ARGUMENTS COUNTE
    int sq_arg = 0; // ARRAY DIM INDEXR
    int sq_mem = 0; // ARRAY DIM INDEX
    int sq_subarg = 0; // ARRAY DIM INDEX
    int sq_mul = 0; // ARRAY DIM PRODUCT
    unsigned sq_size; // ARRAY SIZE
    unsigned *dims; // ARRAY
    int stack[STACK_DEPTH];
    int sp = 0;

    /* SYM_TAB HELPER VARIABLES */
    char tab_name[LOOP_DEPTH];
    int tab_ind; // INDEX
    int tab_type; // TYPE
    int tab_kind; // KIND
    int func_ind = 0; // FUNCTION INDEX
    SYMBOL_ENTRY *head; // TABLE POINTER
%}

/* POSSIBLE TYPES */
%union {
    int i;
    char *s;
    int ii[2];
    int iii[3];
    unsigned d[MAX_DIM];
}

/* TOKENS */
%token _IF
%token _ELSE
%token _SWITCH
%token _CASE
%token _DEFAULT
%token _BREAK
%token _CONTINUE
%token _RETURN
%token _WHILE
%token _DO
%token _FOR

%token _DEF
%token _NULL
%token _CONST

%token <i> _TYPE

%token _LPAREN
%token _RPAREN
%token _LSQBRACK
%token _RSQBRACK
%token _LBRACKET
%token _RBRACKET
%token _SEMICOLON
%token _COMMA
%token _COLON
%token _ASSIGN

%token _PLUS
%token _MINUS
%token _DIV
%token _MOD
%token _SR
%token _SL
%token _BOR
%token _BXOR
%token _AND
%token _OR

%token _STAR
%token _AMP

%token <i> _ITER
%token <i> _RELOP

%token <s> _ID

%token <s> _INT_NUMBER
%token <s> _UINT_NUMBER
%token <s> _HEX_NUMBER

/* TYPE OF VALUE THAT A GIVEN RULE HAS TO RETURN */
/* POSSIBLE TYPES ARE GIVEN IN THE %union ABOVE */
/* $$ IS USED TO SET A VALUE */
%type <i> type literal function_call ar_op log_op helper_num_exp helper_cond helper_cond_simp mem_map data helper_assign
%type <ii> exp
%type <iii> helper_exp
%type <d> array_member_definition

/* SPECIAL RULES */
%nonassoc ONLY_IF   /* NOT ALWAYS; JUST IN THE CASE THAT THERE IS NO ELSE (HENCE NO _ - ONLY_IF IS NOT A TOKEN) */
%nonassoc _ELSE

%start program

%%
/*
*   USED REGS:
*       ra  -  RETURN ADDRESS FOR JUMPING BACK FROM FUNCTION
*       sp  -  STACK POINTER
*       t0  -  FOR MEMORY STORE
*       t1  -  FOR VALUE OF SOME NUMERICAL EXPRESSION
*       tn  -  TEMP REGS
*       t5  -  ARRAY DIMENSION SIZE
*       t6  -  MAIN START
*       s0  -  FUNCTION RETURN VALUE LOCATION
*       sn  -  TEMP REGS FOR ARRAY MANIPULATION
*       s4  -  SYMBOL POINTER
*       s5  -  TEMP REG FOR POINTER ASSIGNMENT
*       an  -  TEMP REG FOR ARRAY DIMENSIONS
*       s11 -  NEXT CALLED FUNCTION MEMORY BEGINING
*       tp  -  CURRENT FUNCTION MEMORY BEGINING
*       gp  -  CURRENT FUNCTION MEMORY SIZE
*/

/*
*   MEMORY STRUCTURE:
*       tp + 0   -  RETURN ADDRESS
*       tp + 4*n -  DATA
*       tp + gp  -  FUNCTION MEMORY SIZE
*/

/*    WHOLE PROGRAM    */
/* INITIAL REGS CONFIG */
program
    : function
    ;
/*    FUNCTION DEFINITION    */
/* CHECK IF NAME IS FREE, ADD TO SYM_TAB AND JUMP BACK */
function
    : type _ID _LPAREN parameter_list _RPAREN body
    ;
/*  NUMBER  */
/* RETURN INDEX AND IS IT NEW */
literal
    : _INT_NUMBER
    | _UINT_NUMBER
    | _HEX_NUMBER
    ;
/* TYPE */
/* RETURN TYPE */ 
/* MAYBE EXPAND WITH CONST? */
type
    : _TYPE
    ;
/*  NO PARAMETERS OR LIST OF FUNCTIONS PARAMETERS  */
parameter_list
    : /* empty */
    | parameter
/*  LIST OF FUNCTIONS PARAMETERS  */
/* RECURSION, CHEK IF NAME IS FREE AND MAKE NEW SYM_TAB ENTRY */
parameter
    : parameter _COMMA parameter_decl
    | parameter_decl
    ;
/*    PARAM DECLARATION    */
/* CHEK IF NAME IS FREE AND MAKE NEW SYM_TAB ENTRY */
/* TO DO - _STAR _STAR */
parameter_decl
    : type _ID
    | type _STAR _ID
    | type _ID array_member_definition
    | type _STAR _ID array_member_definition
    ;
/*    BODY OF A FUNCTION    */
body
    : _LBRACKET variable_list statement_list _RBRACKET
    ;
/*  LIST OF VARIABLE DECLARATIONS  */
/* RECURSION */
variable_list
    : /* empty */
    | variable_list variable_decl_line
    ;
/*    VARIABLE DECLARATION LINE   */
/* MORE VARS IN ONE LINE */
variable_decl_line
    : type variable_decls _SEMICOLON
    ;
/*    VARIABLE DECLARATIONS   */
/* MORE VARS IN ONE LINE */
variable_decls
    : variable_decls _COMMA variable_decl
    | variable_decl
    ;
/*    VARIABLE DECLARATION    */
/* CHEK IF NAME IS FREE AND MAKE NEW SYM_TAB ENTRY */
/* TO DO - MORE IN ONE LINE */
variable_decl
    : _ID
    | _STAR _ID
    | _ID array_member_definition
    | _STAR _ID array_member_definition
    ;
/*  ARRAY PART IN A DECLARATION  */
/* RETURN DIMENSIONS */
array_member_definition
    : array_member_definition _LSQBRACK literal _RSQBRACK
    | _LSQBRACK literal _RSQBRACK
    ;
/*  LIST OF STATEMENTS  */
/* RECURSION */
statement_list
    : /* empty */
    | statement_list statement
    ;
/*    POSSIBLE STATEMENTS    */
statement
    : compound_statement
    | assignment_statement
    | if_statement
    | return_statement
    | while_statement
    | for_statement
    | function_call _SEMICOLON /* FOR VOID FUNCTIONS */
    ;
//    | _CONTINUE _SEMICOLON
//    | _BREAK _SEMICOLON    
//    | do_while_statement
//    | switch_statement

/*    COMPOUND STATMENT    */
compound_statement
    : _LBRACKET statement_list _RBRACKET
    ;
/*    ASSIGNMENT    */
/* SAVE IN MEMORY */
/* TO DO -- VAR vs PAR */
assignment_statement
    : helper_assign num_exp _SEMICOLON
    | helper_assign _AMP mem_map _SEMICOLON
    | data _ITER _SEMICOLON
    | _ITER data _SEMICOLON
    ;
helper_assign
    : data _ASSIGN
    ;
/*  MEMORY MAP  */
/* CHECK IF EXISTS AND RETURN ITS MEMORY_MAP */
mem_map
    : _ID array_member
data
    : mem_map
    | _STAR _ID array_member
    ;
/*  ARRAY PARAMETERS  */
/* STORE IN a LOCATION */
/* TO DO -- BE CAREFULL WITH function_call: MUSN'T BE VOID*/
array_member
    : /* empty */
    | array_mem
    ;
array_mem
    : array_mem _LSQBRACK num_exp _RSQBRACK
    | _LSQBRACK num_exp _RSQBRACK
    ;
/*  ARITHMETICAL OPERATIONS  */
/* RETURN OPERATION TYPE */
ar_op
    : _PLUS
    | _MINUS
    | _STAR
    | _DIV
    | _MOD
    | _SR
    | _SL
    | _AMP
    | _BOR
    | _BXOR
    ;
/*  LOGIAL OPERATION  */
/* RETURN OPERATION TYPE */
log_op
    : _AND
    | _OR
    ;
/*  NUMERICAL EXPRESSION  */
/* PUT RESULT IN t1 */
num_exp
    : exp
    | helper_exp exp
    | helper_exp _LPAREN num_exp _RPAREN
    | helper_num_exp exp
    | helper_num_exp _LPAREN num_exp _RPAREN
    | _PLUS exp
    | _MINUS exp
    | data _ITER
    | _ITER data
    ;
/* HELPER */
/* PUT ON STACK INDEX */
helper_exp
    : exp ar_op
    ;
/* HELPER */
/* PUSH ON STACK */
helper_num_exp
    : _LPAREN num_exp _RPAREN ar_op
    ;
/* ALLOWED ELEMENTS OF num_exp */
/* CHECK FORN TYPE AND RETURN VALUE/INDEX AND TYPE */
exp
    : literal
    | data
    | function_call
    ;
/*    FUNCTION CALL    */
/* CHECK IF EXISTS, CONTEXT SWITCH, JUMP AND RETURN */
function_call
    : _ID _LPAREN argument_list _RPAREN
    ;
/* ARGUMENTS OF A FUNCTION CALL */
/* RECURSION */
argument_list
    : /* empty */
    | argument
    ;
/* ARGUMENTS OF A FUNCTION CALL */
/* RECURSION */
argument
    : argument _COMMA argument_type
    | argument_type
    ;
/* ARGUMENT TYPES OF A FUNCTION CALL */
argument_type
    : num_exp
    | _AMP mem_map
    ;
/*    IF STATEMENT    */
/* BRANCH TO LABEL */
if_statement
    : helper_if %prec ONLY_IF
    |  helper_if _ELSE statement
    ;
/* HELPER */
/* LOOP BRANCH AND EXIT LABLE */
helper_if
    : _IF _LPAREN condition _RPAREN statement
    ;
/*  IF CONDTITIONS  */
condition
    : rel_exp
    | cond_cplx
    ;
/* MORE THAN ONE CONDITION */
/* SET t1 TO 1 OR 0, DEPENDING ON THE OUTCOME OF THE EXPRESSION */
cond_cplx
    : helper_cond_simp rel_exp
    | helper_cond_simp _LPAREN cond_cplx _RPAREN
    | helper_cond rel_exp
    | helper_cond _LPAREN cond_cplx _RPAREN
    ;
/*  HELPER  */
/* STORE IN TEMP REG */
helper_cond_simp
    : rel_exp log_op
    ;
/*  HELPER  */
/* STORE IN TEMP REG */
helper_cond
    : _LPAREN cond_cplx _RPAREN log_op
    ;
/*   REALATIONAL EXPRESSION   */
/* SET t1 TO 1 OR 0, DEPENDING ON THE OUTCOME OF THE EXPRESSION */
rel_exp
    : num_exp _RELOP num_exp
    | num_exp
    ;
/*    LOOPS    */
/*  WHILE STATEMENT  */
while_statement
    : _WHILE _LPAREN condition _RPAREN statement
    ;
/*  FOR STATEMENT  */
/* LABLE START AND BRANCH */
for_start
    : _FOR _LPAREN assignment_statement condition _SEMICOLON
    ;
/* ASSIGN, EXECUTE, BRANCH AND LABEL EXIT */
for_statement 
    : for_start data _ASSIGN num_exp _RPAREN statement
    | for_start data _ITER _RPAREN statement
    | for_start _ITER data _RPAREN statement
    ;
/*    RETURN STATMENT    */ 
/* EITHER retur x; OR return; */
/* CHECK TYPE, SET THE VALUE AND JUMP */
return_statement
    : _RETURN num_exp _SEMICOLON
    | _RETURN _SEMICOLON
    ;

/* TO BE ADDED */

/* SWITCH STATEMENT */
/* TO BE DELT WITH -- NO ACTION ON SYM_TAB (ONLY statement CHANGES SYM_TAB) */
/*
switch_statement
    : _SWITCH _LPAREN num_exp _RPAREN _LBRACKET case_list _RBRACKET
    ;
*/
/* LIST OF CASES */
/* TO BE DELT WITH */
/*
case_list
    : case_list case_statement
    | case_statement
    ;
*/
/* CASE STATEMENT */
/* TO BE DELT WITH */ /* ALLOW ONLY ONE default */
/*
case_statement
    : _CASE num_exp _COLON case_block
    | _DEFAULT _COLON case_block
    ;
*/
/* LIST OF ALLOWED STATEMENTS INSIDE A CASE BLOCK */
/* TO BE DELT WITH */
/*
case_block
    : case_block case_state
    | case_state
    ;
*/
/* ALLOWED STATEMENTS INSIDE A CASE BLOCK */
/* TO BE DELT WITH */
/*
case_state
    : assignment_statement
    | function_call _SEMICOLON // FOR VOID FUNCTIONS
    | _CONTINUE _SEMICOLON
    | _BREAK _SEMICOLON
    ;
*/
/* DO WHILE STATEMENT */
/* TO BE DELT WITH -- NO ACTION ON SYM_TAB (ONLY statement CHANGES SYM_TAB) */
/*
do_while_statement
    : _DO statement _WHILE _LPAREN condition _RPAREN _SEMICOLON
    ;
*/
/* SIMPLE ASSIGN STATEMENTS */
/* TO BE DELT WITH */
/* change_statement
    : data _ASSIGN num_exp
        {
            printf("add t0, x0, t1\n"); // PUT num_exp ON t1
            printf("sw t0, %d, x0\n", 4*$1); // 4*$1 IS A SIMPLE MAP: SYM_TAB -> DATA MEMORY
        }
    | data _ASSIGN _AMP data
        {
            printf("addi t0, x0, %d\n", 4*$1);
            printf("sw t0, %d, x0\n", 4*$1);
        }
    | data _ITER
        {
            if($2 == INC){
                printf("addi t0, t0, 1\n");
            }
            else{
                printf("addi t1, x0, 1\n");
                printf("sub t0, t0, t1\n");
            }
            printf("sw t0, %d, x0\n", 4*$1);
        }
    | _ITER data
        {
            if($1 == INC){
                printf("addi t0, t0, 1\n");
            }
            else{
                printf("addi t1, x0, 1\n");
                printf("sub t0, t0, t1\n");
            }
            printf("sw t0, %d, x0\n", 4*$2);
        }
    ; */
%%

int yyerror(char *s){
    fprintf(stderr, "\nline %d: ERROR: %s\n", yylineno, s);
    error_count++;
    return 0;
}
int main(){
    int syntax_error;
    SYMBOL_ENTRY *head;
    
    init_symtab(&head);
    
    printf("jal start\n");

    /* INIT LOOP LABELS */
    for(for_depth = 0; for_depth < LOOP_DEPTH; for_depth++){
        for_layer[for_depth] = 0;
    }
    for_depth = 0;

    for(while_depth = 0; while_depth < LOOP_DEPTH; while_depth++){
        while_layer[while_depth] = 0;
    }
    while_depth = 0;

    for(if_depth = 0; if_depth < LOOP_DEPTH; if_depth++){
        if_layer[if_depth] = 0;
    }
    if_depth = 0;

    syntax_error = yyparse();
    
    // print_symtab(&head);
    
    destroy_list(&head);

    if(syntax_error)
        return -1;
    else
        return error_count;
}
    
