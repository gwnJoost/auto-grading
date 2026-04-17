{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Parse where

import Data.String( IsString(..) )
import Data.Char
import Data.List
import Data.Array (elems)

import Parse.Token
import Parse.Lex

import Predicate
import Set
}

%name propParser PropFormula
%name setParser Set
%name setConstParser SetConst
%tokentype { Token AlexPosn }
%error { parseError }

%monad { ParseResult } { >>= } { Right }

%token
  TOP    { TokenTop    _ }
  BOT    { TokenBot    _ }
  '('    { TokenOB     _ }
  ')'    { TokenCB     _ }
  '&'    { TokenCon    _ }
  '|'    { TokenDis    _ }
  '=>'   { TokenImpl   _ }
  '<->'  { TokenEqui   _ }
  '~'    { TokenNeg    _ }
  '{'    { TokenOCB    _ }
  '}'    { TokenCCB    _ }
  ','    { TokenComma  _ }
  'U'    { TokenUnion  _ }
  'I'    { TokenIntersect  _ }
  '\\'   { TokenDiff   _ }
  INT    { TokenInt $$ _ }

%right '<->'
%right '=>'
%left '|'
%left '&'
%left '~'

%nonassoc '\\'
%nonassoc 'U'
%nonassoc 'I'

%%

PropFormula : TOP { Top }
            | BOT { Bot }
            | '(' PropFormula ')' { $2 }
            | '~' PropFormula { Neg $2 }
            | PropFormula '=>' PropFormula {Impl $1 $3 }
            | PropFormula '&' PropFormula {And $1 $3 }
            | PropFormula '|' PropFormula {Or $1 $3 }
            | PropFormula '<->' PropFormula {Xor $1 $3 }
            | INT {P $1 }

Set
  : '{' '}'              { S [] }

SetConst : Set 'U' Set {U $1 $3}
         | Set 'I' Set {I $1 $3}
         | Set '\\' Set {Diff $1 $3} 

{

type ParseResult a = Either String a

parseError :: [Token AlexPosn] -> ParseResult a
parseError _ = Left "parse error"

parsePropFormula :: String -> Either String PropFormula
parsePropFormula s = propParser (alexScanTokens s)

}