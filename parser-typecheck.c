/**@<parser.c>::**/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <macros.h>
#include <parser.h>
#include <tokens.h>
#include <keywords.h>
#include <symtab.h>
#include <typecheck.h>
#include <pseudocode.h>

int             lookahead;

/** CONTROL OPERATIONS **/
void
match(int expected)
{
    if (expected == lookahead) {
        lookahead = gettoken(target);
    } else {
        fprintf(stderr, "token mismatch... exiting\n");
    }
}

void
start(void)
{
    lookahead = gettoken(target);
    mypas();
    match(EOF);
}

































/** MAIN BODY DEFINITION **/

/*
 * mypas -> programID declarations blockstmt '.' 
 */
void
mypas(void)
{
    programID();
    declarations();
    blockstmt();
    match('.');
}
/*
 * programID -> [ PROGRAM ID ';' ] 
 */
void
programID(void)
{
    if (lookahead == PROGRAM) {
        match(PROGRAM);
        match(ID);
        match(';');
    }
}

































/** DECLARATIONS DEFINITION **/
/*
 * declarations -> varspecs sbpspecs 
 */
void
declarations(void)
{
    varspecs();
    sbpspecs();
}

#define         VARIABLE        1
#define         FORMPARM        2

void
varspecs(void)
{
     /**/ int       initial,
                    final /**/;
    while (lookahead == VAR) {
        match(VAR);
        do {
             /**/ initial = symtab_nextentry /**/;
             /**/ final = symtab_add(lexeme) /**/;
            match(ID);
            while (lookahead == ',') {
                match(',');
                 /**/ final = symtab_add(lexeme) /**/;
                match(ID);
            }
            match(':');
            typespec(VARIABLE, initial, final);
            match(';');
        } while (lookahead == ID);
    }
}

int
parmpecs(void)
{
     /**/ int       initial,
                    final /**/;
  _parmentry:
    if (lookahead == VAR) {
        match(VAR);
    }
    /**/ initial = symtab_nextentry /**/;
     /**/ final = symtab_add(lexeme) /**/;
    match(ID);
    while (lookahead == ',') {
        match(',');
         /**/ final = symtab_add(lexeme) /**/;
        match(ID);
    }
    match(':');
    typespec(FORMPARM, initial, final);
    if (lookahead == ';') {
        match(';');
        goto _parmentry;
    }
    return final - initial;
}

int
formalparm(void)
{
    int             frame;
    if (lookahead == '(') {
        match('(');
         /**/ frame = symtab_nextentry /**/;
        parmpecs();
        match(')');
    }
    return frame;
}

void
sbpspecs(void)
{
     /**/ int       isfunc = lookahead == FUNCTION /**/;
     /**/ char      subprogname[MAXIDLEN + 1] /**/;
     /**/ int       parmframe /**/;
     /**/ int       subprogframe /**/;
     /**/ int       dtatype /**/;
  _sbpcheck:
    switch (lookahead) {
    case FUNCTION:
    case PROCEDURE:
         /**/ lexlevel++ /**/;
        match(lookahead);
         /**/ strcpy(subprogname, lexeme) /**/;
        match(ID);
         /**/ parmframe = /**/ formalparm();
         /**/ subprogframe = symtab_add(subprogname);
        symtab[subprogframe].parmframe = parmframe /**/;
        if (isfunc) {
            match(':');
             /**/ dtatype = /**/ smptype();
            symtab[subprogframe].objtype = OBJFUNC;
            symtab[subprogframe].dtatype = dtatype /**/;
        } else {
            symtab[subprogframe].objtype = OBJPROC;
        }
        match(';');
        varspecs();
        blockstmt();
        match(';');
         /**/ lexlevel-- /**/;
         /**/ symtab_nextentry = subprogframe /**/;
        goto _sbpcheck;
    }
}
/*
 * typespec -> INTEGER | BOOLEAN | REAL | DOUBLE | CHAR | STRING | ARRAY
 * '[' decimal { ',' decimal } ']' OF typespec 
 */
/*
 * 0 -> void
 * 1 -> integer
 * 2 -> real
 * 3 -> double
 * 4 -> char
 * 5 -> text
 * 6 -> boolean
 */
int
smptype(void)
{
     /**/ int       tp /**/;
    switch (lookahead) {
    case INTEGER:
        match(lookahead);
        tp = TYPEINT;
        break;
    case REAL:
        match(lookahead);
        tp = TYPEFLT;
        break;
    case DOUBLE:
        match(lookahead);
        tp = TYPEDBL;
        break;
    case CHAR:
        match(lookahead);
        tp = TYPEASC;
        break;
    case TEXT:
        match(lookahead);
        tp = TYPESTR;
        break;
    default:
        match(BOOLEAN);
        tp = TYPELOG;
    }
     /**/ return tp /**/;
}

int
size_of_type(int type)
{
    switch (type) {
    case INTEGER:
        return 4;
    case REAL:
        return 4;
    case DOUBLE:
        return 8;
    case CHAR:
        return 1;
    case TEXT:
        return sizeof(void *);
    default:
        return 4;
    }
}

void
typespec(int context, int initial, int final)
/*
 * context = 1 => variable, either global or local;
 *         = 2 => formal parameter.
 */
{
     /**/ int       objtype = 2 * (context - 1) + (lookahead == ARRAY),
        dtatype /**/;
    /*
     * objtype = 0 => scalar variable;
     *         = 1 => array variable;
     *         = 2 => scalar parameter;
     *         = 3 => array parameter.
     */
     /**/ int       i /**/;
     /**/ dtatype = /**/ smptype();
     /**/ for (i = initial; i < final; i++) {
        symtab[i].objtype = objtype;
        symtab[i].dtatype = dtatype;
        if (lexlevel) {
            int             offset =
                last_local_offset + size_of_type(dtatype);
            sprintf(symtab[i].offset, "-%i(%%ebp)", offset);
            last_local_offset = offset;
        } else {
            sprintf(symtab[i].offset, "_%s", symtab[i].symbol);
        }
    }
 /**/}































/** STATEMENT DEFINITIONS **/

void
stmtlist(void)
{
    stmt();
    while (lookahead == ';') {
        match(';');
        stmt();
    }
}

void
stmt(void)
{
    switch (lookahead) {
    case IF:
        ifstmt();
        break;
    case WHILE:
        whilestmt();
        break;
    case REPEAT:
        repstmt();
        break;
    case ID:
        idstmt();
        break;
    case BEGIN:
        blockstmt();
        break;
    default:
        ;
    }
}

void
blockstmt(void)
{
    match(BEGIN);
    stmtlist();
    match(END);
}

void
ifstmt(void)
{
    match(IF);
    expression(TYPELOG);
    match(THEN);
    stmt();
    if (lookahead == ELSE) {
        match(ELSE);
        stmt();
    }
}

void
whilestmt(void)
{
    match(WHILE);
    expression(TYPELOG);
    match(DO);
    stmt();
}

void
repstmt(void)
{
    match(REPEAT);
    stmtlist();
    match(UNTIL);
    expression(TYPELOG);
}

int
idstmt(void)
{
     /**/ char      objname[MAXIDLEN + 1] /**/;
     /**/ int       objentry = symtab_lookup(strcpy(objname, lexeme)),
        objtype,
        dtatype /**/;
     /**/ if (objentry < 0) {
        fprintf(stderr, "%s not found... exiting\n", objname);
        exit(-9);
    }
    /**/ if ((objtype = symtab[objentry].objtype) == OBJFUNC) {
        fprintf(stderr, "%s cannot exist in this context... exiting\n",
                objname);
        exit(-8);
    }
    /**/ dtatype = symtab[objentry].dtatype /**/;
     /**/ match(ID);
     /**/ if (objtype == OBJPROC) {
         /**/ if (lookahead == '(') {
            match('(');
            exprlist(symtab[objentry].argc, symtab[objentry].argt);
            match(')');
        }
    } else {
         /**/ if (objtype == OBJAVAR || objtype == OBJAPAR) {
             /**/ while (lookahead == '[') {
                match('[');
                expr();
                match(']');
            }
        }
        /**/ if (lookahead == COLONEQ) {
             /**/ int       synth_type /**/;
            match(COLONEQ);
             /**/ if (dtatype < (synth_type = expression(dtatype))) {
                fprintf(stderr,
                        "L-type and R-type are not compatible... exiting\n");
                return -11;
            }
            return typematch(dtatype, synth_type) /**/;
        }
    }
     /**/ return dtatype /**/;
}
































/** EXPRESSION DEFINITION **/

/*
 * exprlist -> expr { ',' expr } 
 */
void
exprlist(int argc, int argt[])
{
     /**/ int       i = 0 /**/;
    if (argc == 0) {
        fprintf(stderr, "no argument allowed... exiting\n");
        exit(-12);
    }
    expression(argt[i]);
     /**/ i++ /**/;
    while (lookahead == ',') {
        match(',');
        if (i == argc) {
            fprintf(stderr,
                    "maximum number of arguments exceeded... exiting\n");
            exit(-13);
        }
        expression(argt[i]);
         /**/ i++ /**/;
    }
    if (i < argc) {
        fprintf(stderr, "missing arguments... exiting\n");
        exit(-14);
    }
}

int
isrelop(void)
{
    int             great,
                    less;
    if ((great = lookahead == '>') || (less = lookahead == '<')) {
        match(lookahead);
        if (lookahead == '=')
            match('=');
        else if (less && lookahead == '>')
            match('>');
        return 1;
    }
    if (lookahead == '=') {
        match('=');
        return 1;
    }
    return 0;
}

int
expression( /**/ int inherited_type /**/)
{
     /**/ int       synthesized_type = /**/ expr();
    if (isrelop()) {
         /**/ synthesized_type = typematch(synthesized_type, expr()) /**/;
         /**/ if (synthesized_type < 0) {
            fprintf(stderr, "type mismatch in relational expression\n");
            exit(-7);
        }
        /**/ /**/ synthesized_type =
            TYPELOG /*resulting boolean from comparison */ ;
    }
    /**/ return typematch(synthesized_type, inherited_type) /**/;
}

int
expr(void)
{
     /**/ int       synth_type = 0 /**/;
    switch (lookahead) {
    case '-':
         /**/ synth_type = TYPEINT /**/;
        match('-');
        break;
    case NOT:
         /**/ synth_type = TYPELOG /**/;
        match(NOT);
    }

     /**/ if ((synth_type = typematch(synth_type, term())) < 0) {
        fprintf(stderr, "operand type mismatches unary operator\n");
        exit(-5);
    }
    /**/ while (synth_type = typematch(synth_type, isaddop())) {
         /**/ if ((synth_type = typematch(synth_type, term())) < 0) {
            fprintf(stderr, "operand type mismatches additive operator\n");
            exit(-5);
        }
    /**/}
     /**/ return synth_type /**/;
}

int
isnegate(void)
{
    return lookahead == '-' || lookahead == NOT;
}

int
isaddop(void)
{
    switch (lookahead) {
    case '+':
    case '-':
        match(lookahead);
         /**/ return 1 /**/;
    case OR:
        match(OR);
         /**/ return 4 /**/;
    }
     /**/ return 0 /**/;
}

int
ismulop(void)
{
    switch (lookahead) {
    case '*':
    case '/':
    case DIV:
    case MOD:
        match(lookahead);
         /**/ return 1 /**/;
    case AND:
        match(AND);
         /**/ return 4 /**/;
    }
     /**/ return 0 /**/;
}

int
term(void)
{
     /**/ int       synth_type = factor() /**/;
    while (synth_type = typematch(synth_type, ismulop())) {
         /**/ if ((synth_type = typematch(synth_type, factor())) < 0) {
            fprintf(stderr, "operand type mismatches product operator\n");
            exit(-5);
        }
    /**/}
     /**/ return synth_type /**/;
}

#define SMBNFND         -9
#define ILEGCALL        -8

int
idfactor(void)
{
     /**/ char      objname[MAXIDLEN + 1] /**/;
     /**/ int       objentry = symtab_lookup(strcpy(objname, lexeme)),
        objtype,
        dtatype /**/;
     /**/ if (objentry < 0) {
        fprintf(stderr, "%s not found... exiting\n", objname);
        exit(SMBNFND);
    }
    /**/ if ((objtype = symtab[objentry].objtype) == OBJPROC) {
        fprintf(stderr, "%s cannot exist in this context... exiting\n",
                objname);
        exit(ILEGCALL);
    }
    /**/ dtatype = symtab[objentry].dtatype /**/;
     /**/ match(ID);
     /**/ if (objtype == OBJFUNC) {
         /**/ if (lookahead == '(') {
            match('(');
            exprlist(symtab[objentry].argc, symtab[objentry].argt);
            match(')');
        }
    } else {
         /**/ if (objtype == OBJAVAR || objtype == OBJAPAR) {
           /**/ _open_bracket:
            match('[');
            expr();
            match(']');
            if (lookahead == '[')
                goto _open_bracket;
        }
        /** next 3 lines abstracts idfactor -> variable := expression **/
         /**/ if (lookahead == COLONEQ) {
            match(COLONEQ);
             /**/ return typematch(dtatype, expression(dtatype)) /**/;
        }
    }
     /**/ return dtatype /**/;
}

factor(void)
{
     /**/ int       factor_type /**/;
    switch (lookahead) {
    case ID:
         /**/ factor_type = /**/ idfactor();
        break;
    case UINT:
         /**/ factor_type = /**/ 1;
        match(lookahead);
        break;
    case UFLT:
         /**/ factor_type = /**/ 2;
        match(lookahead);
        break;
    case UDBL:
         /**/ factor_type = /**/ 3;
        match(lookahead);
        break;
    case ASCII:
         /**/ factor_type = /**/ 4;
        match(lookahead);
        break;
    case STRING:
         /**/ factor_type = /**/ 5;
        match(lookahead);
        break;
    case TRUE:
    case FALSE:
         /**/ factor_type = /**/ 6;
        match(lookahead);
        break;
    default:
        match('(');
         /**/ factor_type = /**/ expression(0);
        match(')');
    }
     /**/ return factor_type /**/;
}
