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
\\				{ \_ -> TkAbs }
$alpha [$alpha $digit \_ \']*
				{ \s -> TkVar s }
\( 				{ \_ -> TkLeftPar }
\) 				{ \_ -> TkRightPar }
\.				{ \_ -> TkDot }
\:				{ \_ -> TkColon }
\->				{ \_ -> TkArrow }
\*              { \_ -> TkStar }
\@              { \_ -> TkPi }
\Π              { \_ -> TkPi }

{

data Token =
  TkAbs |
  TkVar String |
  TkLeftPar |
  TkRightPar |
  TkDot |
  TkColon |
  TkArrow |
  TkStar |
  TkPi
  deriving ( Eq, Show )

tkVarName :: Token -> String
tkVarName ( TkVar name ) = name
tkVarName tk = error ( "Trying to get a name of " ++ show tk )

}