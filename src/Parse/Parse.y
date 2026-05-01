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
%name itemParser Item
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
  STR    { TokenString $$ _ }

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
  | '{' Items '}'        { S $2 }
  | Set 'U' Set          {U $1 $3}
  | Set 'I' Set          {I $1 $3}
  | Set '\\' Set         {Diff $1 $3} 

Items
  : Item                 { [$1] }
  | Item ',' Items       { $1 : $3 }

Item
  : INT                   { Int $1 }
  | Set                   { Set $1 }
{

type ParseResult a = Either (Int,Int) a

parseError :: [Token AlexPosn] -> ParseResult a
parseError [] = Left (1,1)
parseError (t:ts) = Left (lin, col) where
  (AlexPn _ lin col) = apn t

parsePropFormula :: String -> Either (Int, Int) PropFormula
parsePropFormula s = propParser (alexScanTokens s)

parseSet :: String -> Either (Int, Int) Set
parseSet s = setParser (alexScanTokens s)
}