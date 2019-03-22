%{
  open LMJ
%}

%token <int32> INT_CONST
%token <bool> BOOL_CONST
%token INTEGER BOOLEAN
%token <string Location.t> IDENT
%token CLASS PUBLIC STATIC VOID MAIN STRING EXTENDS RETURN
%token AND TIMES PLUS MINUS LT NOT
%nonassoc NOT
%left LBRACKET
%left DOT
%left PLUS
%left MINUS
%left TIMES
%left AND
%nonassoc LT
%token COMMA SEMICOLON
%token ASSIGN
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token THIS NEW DOT LENGTH
%token SYSO
%token IF ELSE WHILE
%token EOF

%type <LMJ.program> program
%start program
%%

program:
| m = main_class; d = list (classDeclaration); EOF
   {
     let c, a, i = m in
     {
       name = c;
       defs = d;
       main_args = a;
       main = i
     }
   }

classDeclaration:
| CLASS; id = IDENT; LBRACE; var = list (varDeclaration); meth = list (methodDeclaration); RBRACE
    {
        (id, {extends = None; attributes = var; methods = meth})
    }
| CLASS; id = IDENT; EXTENDS; id2 = IDENT; LBRACE; var = list (varDeclaration); meth = list (methodDeclaration); RBRACE
    {
        (id, {extends = Some id2; attributes = var; methods = meth})
    }

varDeclaration:
| typ = typ; id = IDENT; SEMICOLON
    { (id, typ) }

typ:
| INTEGER
    { TypInt }
| INTEGER; LBRACKET; RBRACKET
    { TypIntArray }
| BOOLEAN
    { TypBool }
| id = IDENT
    { Typ id }

methodDeclaration:
| PUBLIC; typ = typ; id = IDENT; LPAREN; args = separated_list (COMMA, argument); RPAREN;
LBRACE; varStat = statAux; RETURN; exp = expression; SEMICOLON; RBRACE
    { 
        let var, stat = varStat in
        (id, {formals = args; result = typ; locals = var; body = IBlock stat; return = exp})
    }

statAux:
| var = varDeclaration; rest = statAux
    { let  dec, stat = rest in 
        var::dec, stat
    }
| stat = list (statement)
    { [],stat }

argument:
| typ = typ; id = IDENT
    { (id, typ) }

main_class:
| CLASS; name = IDENT; LBRACE; PUBLIC; STATIC; VOID; MAIN; LPAREN; STRING; LBRACKET; RBRACKET
args = IDENT; RPAREN; LBRACE; state = statement; RBRACE; RBRACE
    { (name, args, state) }

statement:
| LBRACE; stat = list (statement); RBRACE
    { IBlock stat }
| IF; LPAREN; exp = expression; RPAREN; thenn = statement; ELSE; elsee = statement;
    { IIf (exp, thenn, elsee) }
| WHILE; LPAREN; exp = expression; RPAREN; stat = statement;
    { IWhile (exp, stat) }
| SYSO; LPAREN; exp = expression; RPAREN; SEMICOLON
    { ISyso exp }
| id = IDENT; ASSIGN; exp = expression; SEMICOLON
    { ISetVar (id, exp) }
| id = IDENT; LBRACKET; exp1 = expression; RBRACKET; ASSIGN; exp2 = expression; SEMICOLON
    { IArraySet (id, exp1, exp2) }

expression:
|  e = raw_expression
    { Location.make $startpos $endpos e }
| LPAREN; e = expression; RPAREN
    { e }

raw_expression:
| exp1 = expression; op = operator; exp2 = expression
    { EBinOp (op, exp1, exp2) }
| exp1 = expression; LBRACKET; exp2 = expression; RBRACKET;
    { EArrayGet (exp1, exp2) }
| exp1 = expression; DOT; LENGTH;
    { EArrayLength exp1 }
| exp1 = expression; DOT; id = IDENT; LPAREN; exp2 = separated_list (COMMA, expression); RPAREN;
    { EMethodCall (exp1, id, exp2) }
| integer = INT_CONST
    { EConst ( ConstInt integer ) }
| boul = BOOL_CONST;
    { EConst (ConstBool boul) }
| str = IDENT
    { EGetVar str }
| THIS;
    { EThis }
| NEW; INTEGER; LBRACKET; exp = expression; RBRACKET;
    { EArrayAlloc exp }
| NEW; id = IDENT; LPAREN; RPAREN;
    { EObjectAlloc id }
| NOT; exp = expression;
    { EUnOp (UOpNot, exp) }

%inline operator:
| PLUS { OpAdd }
| MINUS { OpSub }
| TIMES { OpMul }
| LT { OpLt }
| AND { OpAnd }