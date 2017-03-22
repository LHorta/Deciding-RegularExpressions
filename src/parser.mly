%{
open RegExp
open Misc
%}

%token <char> CHAR
%token EMPTY EPSILON CHOICE CONCAT SYNC STAR LPAR RPAR
%token EOF EOL

%left CHOICE
%left CONCAT
%left SYNC
%left STAR

%start main
%type <RegExp.regexp_t list> main

%%

main: r = regexlist EOL* EOF { List.rev r }

regexlist:
  r = regex {[r]}
| rl = regexlist  EOL+ r = regex  {r::rl}
| rl = regexlist EOL+ error  { print_syntax_error ($startpos($3)) ; rl }

regex:
  | EPSILON   { Epsilon }
  | EMPTY     { Empty }
  | c = CHAR  { Char c }
  | LPAR r = regex RPAR { r }
  | a = regex CHOICE b = regex { Choice(a, b) }
  | a = regex CONCAT b = regex { Concat(a, b) }
  | r = regex STAR { Star r }
  | a = regex SYNC b = regex { Sync(a,b)}
