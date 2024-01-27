{
module LambdaFrontend.Lexer where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]
$greek = [α-ζΑ-Ζ]
@id = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*

tokens :-

$white+		;
unit            { \_ -> TkUnit }
Unit            { \_ -> TkTUnit }
let             { \_ -> TkLet }
in              { \_ -> TkIn }
\\				{ \_ -> TkAbs }
$alpha [$alpha $digit \_ \']*
				{ \s -> TkVar s }
\( 				{ \_ -> TkLeftPar }
\) 				{ \_ -> TkRightPar }
\.				{ \_ -> TkDot }
\:				{ \_ -> TkColon }
\;              { \_ -> TkSemicolon }
\->				{ \_ -> TkArrow }
\*              { \_ -> TkStar }
\@              { \_ -> TkPi }
\Π              { \_ -> TkPi }
=               { \_ -> TkEq }

{

data Token =
  TkAbs |
  TkVar String |
  TkLeftPar |
  TkRightPar |
  TkDot |
  TkColon |
  TkSemicolon |
  TkArrow |
  TkStar |
  TkPi |
  TkLet |
  TkEq |
  TkIn |
  TkUnit |
  TkTUnit
  deriving ( Eq, Show )

tkVarName :: Token -> String
tkVarName ( TkVar name ) = name
tkVarName tk = error ( "Trying to get a name of " ++ show tk )

}