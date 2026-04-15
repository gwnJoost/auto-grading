{
{-# OPTIONS_GHC -w #-}
{-# OPTIONS_HADDOCK hide #-}

module Predicate.Lex where
import Predicate.Token
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
  "⊤"               { \ p _ -> TokenTop               p }
  "false"           { \ p _ -> TokenBot               p }
  "⊥"               { \ p _ -> TokenBot               p }
  "~"               { \ p _ -> TokenNeg               p }
  "-"               { \ p _ -> TokenNeg               p }
  "¬"               { \ p _ -> TokenNeg               p }
  "&"               { \ p _ -> TokenCon               p }
  "^"               { \ p _ -> TokenCon               p }
  "and"             { \ p _ -> TokenDis               p }
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
  $dig+             { \ p s -> TokenInt (read s)      p }
