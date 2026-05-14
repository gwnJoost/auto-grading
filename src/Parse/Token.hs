module Parse.Token where

data Token a
  = TokenInt    {i::Int,    apn :: a}
  | TokenString {s::String, apn :: a}
  | TokenTop               {apn :: a}
  | TokenBot               {apn :: a}
  | TokenPrp               {apn :: a}
  | TokenNeg               {apn :: a}
  | TokenOB                {apn :: a}
  | TokenCB                {apn :: a}
  | TokenCon               {apn :: a}
  | TokenDis               {apn :: a}
  | TokenImpl              {apn :: a}
  | TokenEqui              {apn :: a}
  | TokenDiaL              {apn :: a}
  | TokenDiaR              {apn :: a}
  | TokenDia               {apn :: a}
  | TokenBoxL              {apn :: a}
  | TokenBoxR              {apn :: a}
  | TokenBox               {apn :: a}
  | TokenUnion             {apn :: a}
  | TokenIntersect         {apn :: a}
  | TokenDiff              {apn :: a}
  | TokenOCB               {apn :: a}
  | TokenCCB               {apn :: a}
  | TokenComma             {apn :: a}
  | TokenPrem              {apn :: a}
  | TokenAss               {apn :: a}
  | TokenImplI             {apn :: a}
  | TokenImplE             {apn :: a}
  | TokenNegE              {apn :: a}
  | TokenNegI              {apn :: a}
  | TokenDNegE             {apn :: a}
  | TokenAndI              {apn :: a}
  | TokenAndE              {apn :: a}
  | TokenOrI               {apn :: a}
  | TokenOrE               {apn :: a}
  | TokenBotE              {apn :: a}
  | TokenDash              {apn :: a}
  | TokenNewLn             {apn :: a}
  | TokenSemiColon         {apn :: a}
  deriving (Eq,Show)