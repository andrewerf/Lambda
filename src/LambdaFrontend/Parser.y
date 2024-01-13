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
'->'		{ TkArrow }
'*'         { TkStar }
'@'         { TkPi }

%%

Expr : LeftExpr Lambda          { TmApp $1 $2 }
     | Lambda                   { $1 }
     | LeftExpr                 { $1 }

LeftExpr : LeftExpr Atom        { TmApp $1 $2 }
         | Atom                 { $1 }

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
