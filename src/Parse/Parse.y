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
import Ndeduction
}

%name propParser PropFormula
%name setParser Set
%name itemParser Item
%name ndParser NDProof
%name ndLParser NDLine
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
  'P'    { TokenPrem   _ }
  'A'    { TokenAss    _ }
  '->I'  { TokenImplI  _ }
  '->E'  { TokenImplE  _ }
  '-I'   { TokenNegI   _ }
  '-E'   { TokenNegE   _ }
  '--E'  { TokenDNegE  _ }
  '^I'   { TokenAndI   _ }
  '^E'   { TokenAndE   _ }
  'vI'   { TokenOrI    _ }
  'vE'   { TokenOrE    _ }
  'BotE' { TokenBotE   _ }
  '-'    { TokenDash   _ }
  '\n'   { TokenNewLn  _ }
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

NDProof
  : NDLines               {Proof $1}

NDLines
  : NDLine                {[$1]}
  | NDLine '\n' NDLines   { $1 : $3 }

NDLine
  : PropFormula 'P' Lst   {L ($1, Premise, $3)}
  | PropFormula 'A' Lst   {L ($1, Assumption, $3)}
  | PropFormula '->I' INT '-' INT Lst   {L ($1, ImplI $3 $5, $6)}
  | PropFormula '->E' INT INT Lst   {L ($1, ImplE $3 $4, $5)}
  | PropFormula '-I' INT INT Lst   {L ($1, NegI $3 $4, $5)}
  | PropFormula '-E' INT INT Lst   {L ($1, NegE $3 $4, $5)}
  | PropFormula '--E' INT Lst   {L ($1, DNegE $3, $4)}
  | PropFormula '^I' INT INT Lst   {L ($1, AndI $3 $4, $5)}
  | PropFormula '^E' INT Lst   {L ($1, AndE $3, $4)}
  | PropFormula 'vI' INT Lst   {L ($1, OrI $3, $4)}
  | PropFormula 'vE' INT INT '-' INT INT '-' INT Lst   {L ($1, OrE $3 $4 $6 $7 $9, $10)}
  | PropFormula 'BotE' INT Lst   {L ($1, BotE $3, $4)}

Lst
  : INT                  {[$1]}
  | INT ',' Lst          {$1 : $3}
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