{
{-# OPTIONS_GHC -w #-}
{-# OPTIONS_HADDOCK hide #-}

module Parse.Lex where
import Parse.Token
}

%wrapper "posn"

$dig = [0-9]      -- digits
$alf = [a-zA-Z] -- alphabetic characters

tokens :-
  -- ignore whitespace:
  $white+           ;
  -- ignore the word "begin"
  "begin"           ;
  -- ignore the word "end"
  "end"             ;
  -- keywords and punctuation:
  "("               { \ p _ -> TokenOB                p }
  ")"               { \ p _ -> TokenCB                p }
  -- Formulas:
  "true"            { \ p _ -> TokenTop               p }
  "T"               { \ p _ -> TokenTop               p }
  "false"           { \ p _ -> TokenBot               p }
  "⊥"               { \ p _ -> TokenBot               p }
  "~"               { \ p _ -> TokenNeg               p }
  "-"               { \ p _ -> TokenNeg               p }
  "¬"               { \ p _ -> TokenNeg               p }
  "&"               { \ p _ -> TokenCon               p }
  "^"               { \ p _ -> TokenCon               p }
  "and"             { \ p _ -> TokenCon               p }
  "|"               { \ p _ -> TokenDis               p }
  "v"               { \ p _ -> TokenDis               p }
  "or"              { \ p _ -> TokenDis               p }
  "=>"              { \ p _ -> TokenImpl              p }
  "→"               { \ p _ -> TokenImpl              p }
  "->"              { \ p _ -> TokenImpl              p }
  "-->"             { \ p _ -> TokenImpl              p }
  "<->"             { \ p _ -> TokenEqui              p }
  "<=>"             { \ p _ -> TokenEqui              p }
  "<-->"            { \ p _ -> TokenEqui              p }
  "↔"               { \ p _ -> TokenEqui              p }
  -- Sets
  "{"               { \ p _ -> TokenOCB               p }
  "}"               { \ p _ -> TokenCCB               p }
  ","               { \ p _ -> TokenComma             p }
  "U"               { \ p _ -> TokenUnion             p }
  "I"               { \ p _ -> TokenIntersect         p }
  "\\"              { \ p _ -> TokenDiff              p }
  -- Natural Deduction
  "P"               { \ p _ -> TokenPrem              p }
  "A"               { \ p _ -> TokenAss               p }
  "->I"             { \ p _ -> TokenImplI             p }
  "->E"             { \ p _ -> TokenImplE             p }
  "-I"              { \ p _ -> TokenNegI              p }
  "-E"              { \ p _ -> TokenNegE              p }
  "¬I"              { \ p _ -> TokenNegI              p }
  "¬E"              { \ p _ -> TokenNegE              p }
  "--E"             { \ p _ -> TokenDNegE             p }
  "¬¬E"             { \ p _ -> TokenDNegE             p }
  "^I"              { \ p _ -> TokenAndI              p }
  "^E"              { \ p _ -> TokenAndE              p }
  "vI"              { \ p _ -> TokenOrI               p }
  "VE"              { \ p _ -> TokenAndI              p }
  "BotE"            { \ p _ -> TokenBotE              p }
  "⊥E"              { \ p _ -> TokenBotE              p }
  -- other
  '-'               { \ p _ -> TokenDash              p }
  '\n'              { \ p _ -> TokenNewLn             p }
  -- numbers
  $dig+             { \ p s -> TokenInt (read s)      p }
  $alf+             { \ p s -> TokenString s          p }

