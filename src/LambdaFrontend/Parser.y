{
module LambdaFrontend.Parser ( parse ) where

import LambdaFrontend.Lexer
import LambdaFrontend.AST

}

%name parseLambda
%tokentype { Token }
%error { parseError }

%token
'\\'		{ TkAbs }
var			{ TkVar _ }
'.'			{ TkDot }
'('			{ TkLeftPar }
')'			{ TkRightPar }
':'			{ TkColon }
';'			{ TkSemicolon }
'->'		{ TkArrow }
'*'         { TkStar }
'@'         { TkPi }
let         { TkLet }
'='         { TkEq }
in          { TkIn }

%%

Expr : LeftExpr Lambda          { TmApp $1 $2 }
     | Lambda                   { $1 }
     | LeftExpr                 { $1 }
     | Let                      { $1 }

LeftExpr : LeftExpr Atom        { TmApp $1 $2 }
         | Atom                 { $1 }

Let : let var '=' Expr in Expr  { TmLetIn ( tkVarName $2 ) $4 $6 }
    | let var '=' Expr          { TmLet ( tkVarName $2 ) $4 Nothing } -- I know it is a shift/reduce conflict, but Happy prefers shift over reduce and this is what we need here
    | let var '=' Expr ';'      { TmLet ( tkVarName $2 ) $4 Nothing }
    | let var '=' Expr ';' Let  { TmLet ( tkVarName $2 ) $4 ( Just $6 ) }

Lambda : '\\' var ':' Expr '.' Expr
                                { TmAbs ( tkVarName $2 ) $4 $6 }
       | '@' var ':' Expr '.' Expr
                                { TmPi ( tkVarName $2 ) $4 $6 }

Atom : '(' Expr ')'             { $2 }
     | var                      { TmVar ( tkVarName $1 ) }
     | '*'                      { TmStar }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens

parse = parseLambda.alexScanTokens
}
