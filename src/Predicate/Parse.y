{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
module Predicate.Parse where

import Data.String( IsString(..) )
import Data.Char
import Data.List

import Predicate.Token
import Predicate.Lex

import Predicate
}

%name propParser
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
  INT    { TokenInt $$ _ }


%right '<->'
%right '=>'
%left '|'
%left '&'
%left '~'

%%

PropFormula : TOP { Top }
            | BOT { Bot }
            | '(' PropFormula ')' { $2 }
            | '~' PropFormula { Neg $2 }
            | PropFormula '=>' PropFormula { Impl $1 $3 }
            | PropFormula '&' PropFormula { And $1 $3 }
            | PropFormula '|' PropFormula {Or $1 $3 }
            | PropFormula '<->' PropFormula {Xor $1 $3 }
            | INT {P $1 }

{

type ParseResult a = Either String a

parseError :: [Token AlexPosn] -> ParseResult a
parseError _ = Left "parse error"

parsePropFormula :: String -> Either String PropFormula
parsePropFormula s = propParser (alexScanTokens s)
}