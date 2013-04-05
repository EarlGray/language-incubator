%{
#include <stdlib.h>
    
int yylex(void);

#define null    ((void *)0)

typedef unsigned   uint, type_t, atomic_t;
typedef char string_t;
//typedef wchar_t string_t;

struct entity;
typedef struct entity entity_t, *entity_pt;

entity_t * make_list_from_array(entity_t *ent);

entity_t *lisp_cons(entity_t *ent1, entity_t *ent2);
entity_t *lisp_car(entity_t *ent);
entity_t *lisp_cdr(entity_t *ent);
entity_t *lisp_append(entity_t *list1, entity_t *list2);
entity_t *lisp_isnil(entity_t *ent);

/**********************************************************
    Entity can be:
        atom (string | number | void * | whatever)
        symbol (string *name, entity *bound)
        cons (entity *car, entity *cdr)
    It is 
        16b in 32bit machine and
        24b on 64bit machine

**********************************************************/

struct entity {
    type_t type;
    uint used;
    union {
        struct {
            string_t *name;
            entity_t *bound;
        } symbol;
        struct {
            entity_t *car;
            entity_t *cdr;
        } cons;
        struct {
            void *v1;
            void *v2;
        } atom;
   } as;
};

inline entity_pt copy(entity_pt e) {
    entity_pt new_e = (entity_pt)malloc(sizeof(e));
    // enter_critcal()
    e->used ++;
    *new_e = *e;
    // leave critical()
    return new_e;
}

inline entity_pt retain(entity_pt e) {
    ++ e->used;
    return e;
}

inline entity_pr release(entity_pt e) {
    -- e->used;
    if (e->used > 0) 
        return e;

    release_deps(e);
    free(e);
    return null;
}

#define ENT_CONS        0
#define ENT_SYMBOL      1
#define ENT_STRING      2
#define ENT_NUMBER      3
#define ENT_CFUNC       4

/******* Built-in forms ********************************/
#define BIF_CONS    0
#define BIF_CAR     1
#define BIF_CDR     2
#define BIF_ISNULL  3

const entity_t builtin_forms[] = {
    { .type = ENT_CFUNC, .used = 1, .as.atom = { .v1 = (void *)make_cons, .v2 = (void *)2 } },
    { .type = ENT_CFUNC, .used = 1, .as.atom = { .v1 = (void *)lisp_car, .v2 = (void *)1 } },
    { .type = ENT_CFUNC, .used = 1, .as.atom = { .v1 = (void *)lisp_cdr, .v2 = (void *)1 } },
    { .type = ENT_CFUNC, .used = 1, .as.atom = { .v1 = (void *)lisp_isnil, v2 = (void *)1 } },
    { .type = ENT_CFUNC, .used = 0, .as.atom = { .v1 = null, .v2 = null } },
};

/******* Symbols ***************************************/
const entity_t builtins[] = {
    { .type = ENT_SYMBOL, .used = 1, .as.symbol = { .name = "cons", .bound = &(builtin_forms[BIF_CONS]) } },
    { .type = ENT_SYMBOL, .used = 1, .as.symbol = { .name = "car", .bound = &(builtin_forms[BIF_CAR]) } },
    { .type = ENT_SYMBOL, .used = 1, .as.symbol = { .name = "cdr", .bound = &(builtin_forms[BIF_CDR]) } },
    { .type = ENT_SYMBOL, .used = 1, .as.symbol = { .name = "nil?", .bound = &(builtin_forms[BIF_ISNULL]) } },
};

entity_t global_env = {
    .type = ENT_CONS,  
    .used = 1,
    .as.cons = { 
        .car = &global_env, 
        .cdr = null, // to be initialized with make_list_from_array(builtins) 
    },
};

entity_pt make_list_from_array(entity_t *ent) {
    entity_pt *current = builtins;
    while () {
        
    }
}

#define YYSTYPE     entity_pt 

entity_pt lisp_cons(entity_pt e1, entity_pt e2) {
    entity_pt e = (entity_pt)malloc(sizeof(entity_t));
    e->type = ENT_CONS;
    e->used = 0;
    e->as.cons.car = retain(e1);
    e->as.cons.cdr = retain(e2);
    return e;
}

%}

%token L_LP         /* left parenthesis  */
%token L_RP         /* right parenthesis */
%token L_SYMBOL     /* any symbol        */
%token L_SPECFORM   /* sepcial form      */
%token L_QUOTER     /* , `  '            */
%token L_STRING     /* "string"          */
%token L_NUMBER     /* 42                */
%token L_COMMENT    /* ;; comment        */

%%

input
    :   atom        {   }
    |   sexp        {   }
;

atom
    :   L_SPECFORM      {       }
    |   L_SYMBOL        {       }
    |   L_NUMBER        {       }
    |   L_STRING        {       }
;

quote
    :   L_QUOTER atom   {       }
    |   L_QUOTER sexp   {       }
;

sexp_body
    :   atom                {   }
    |   atom sexp_body      {   }
;

sexp
    :   L_LP sexp_body L_RP   {       }
    |   L_LP L_RP       {       }
;

%%

void init(void) {
    lisp_append(global_env, make_list_from_array(builtins));
}

void yyerror(const char *error) {
    perror(error);
}

int main() {
    init();
    yyparse();
    return 0;
}
