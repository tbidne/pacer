module Pacer.Command.Chart.Data.Expr
  ( -- * Expressions
    Expr (..),
    FilterExpr,
    eval,

    -- ** Parsing
    ExprToken (..),
    exprLexer,
    exprParser,

    -- * Filtering
    FilterType (..),
    FilterOp (..),

    -- * Misc
    lexParse,
  )
where

import Control.Monad.Combinators.Expr (Operator)
import Control.Monad.Combinators.Expr qualified as Combs
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.List qualified as L
import Data.Text qualified as T
import Pacer.Class.Parser (MParser, Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Time.Moment (Moment)
import Pacer.Data.Distance (SomeDistance)
import Pacer.Data.Duration (Duration)
import Pacer.Data.Pace (SomePace)
import Pacer.Prelude
import Text.Megaparsec
  ( Parsec,
    PosState (PosState),
    TraversableStream (reachOffset),
    VisualStream,
    (<?>),
  )
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-------------------------------------------------------------------------------
--                                  FilterOp                                 --
-------------------------------------------------------------------------------

-- | Operator for filter comparisons.
data FilterOp
  = FilterOpEq
  | FilterOpNeq
  | FilterOpLte
  | FilterOpLt
  | FilterOpGte
  | FilterOpGt
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Display FilterOp where
  displayBuilder = \case
    FilterOpEq -> "="
    FilterOpNeq -> "/="
    FilterOpLte -> "<="
    FilterOpLt -> "<"
    FilterOpGte -> ">="
    FilterOpGt -> ">"

instance Parser FilterOp where
  parser =
    asum
      -- NOTE: These use megaparsec's variants rather than ours defined in
      -- Pacer.Class.Parser which parse all trailing whitespace too.
      -- We currently need this since it is used in (FilterType a)'s Parser
      -- instance, and we require space1 there. It would be nice to figure
      -- out a more consistent implementation here.
      [ MPC.string "<=" $> FilterOpLte,
        MPC.char '<' $> FilterOpLt,
        MPC.char '=' $> FilterOpEq,
        MPC.string "/=" $> FilterOpNeq,
        MPC.string ">=" $> FilterOpGte,
        MPC.char '>' $> FilterOpGt
      ]

-------------------------------------------------------------------------------
--                                 FilterType                                --
-------------------------------------------------------------------------------

-- | Ways in which we can filter runs.
data FilterType a
  = FilterDistance FilterOp (SomeDistance a)
  | FilterDuration FilterOp (Duration a)
  | -- | Filters by label equality.
    FilterLabel Text
  | FilterDate FilterOp Moment
  | FilterPace FilterOp (SomePace a)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance
  ( AMonoid a,
    Display a,
    Fromℤ a,
    MSemigroup a,
    Ord a,
    Show a,
    Toℚ a
  ) =>
  Display (FilterType a)
  where
  displayBuilder = \case
    FilterDistance op d ->
      mconcat
        [ "distance ",
          displayBuilder op,
          " ",
          displayBuilder d
        ]
    FilterDuration op d ->
      mconcat
        [ "duration ",
          displayBuilder op,
          " ",
          displayBuilder d
        ]
    FilterLabel t ->
      mconcat
        [ "label ",
          displayBuilder t
        ]
    FilterDate op m ->
      mconcat
        [ "datetime ",
          displayBuilder op,
          " ",
          displayBuilder m
        ]
    FilterPace op d ->
      mconcat
        [ "pace ",
          displayBuilder op,
          " ",
          displayBuilder d
        ]

instance
  ( Fromℚ a,
    Ord a,
    Parser a,
    Semifield a,
    Show a
  ) =>
  Parser (FilterType a)
  where
  parser =
    asum
      [ parseLabel <?> "label",
        parseDist <?> "distance",
        parseDuration <?> "duration",
        parsePace <?> "pace",
        parseDate <?> "datetime"
      ]
    where
      parseLabel = do
        void $ MPC.string "label"
        MPC.space1
        FilterLabel <$> MP.takeWhile1P (Just "string") (const True)

      parseDate = parsePred "datetime" FilterDate
      parseDist = parsePred "distance" FilterDistance
      parseDuration = parsePred "duration" FilterDuration
      parsePace = parsePred "pace" FilterPace

      parsePred s cons = do
        MPC.string s
        MPC.space1
        op <- parser
        MPC.space1
        d <- parser
        pure $ cons op d

-------------------------------------------------------------------------------
--                                    Expr                                   --
-------------------------------------------------------------------------------

-- | Expressions upon which we can filter
data Expr a
  = -- | "expr"
    Atom a
  | -- | "not (expr)"
    Not (Expr a)
  | -- | "or (expr) (expr)"
    Or (Expr a) (Expr a)
  | -- | "and (expr) (expr)"
    And (Expr a) (Expr a)
  deriving stock (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving anyclass (NFData)

instance (Display a) => Display (Expr a) where
  displayBuilder = go
    where
      go = \case
        Atom x -> displayBuilder x
        Not e -> "not (" <> go e <> ")"
        Or e1 e2 ->
          mconcat
            [ "(",
              go e1,
              ") or (",
              go e2,
              ")"
            ]
        And e1 e2 ->
          mconcat
            [ "(",
              go e1,
              ") and (",
              go e2,
              ")"
            ]

-- | Alias for a filter expression.
type FilterExpr a = Expr (FilterType a)

instance (Parser a) => FromJSON (Expr a) where
  parseJSON = asnWithText "Expr" (failErr . lexParse)

eval :: (a -> Bool) -> Expr a -> Bool
eval p = runIdentity . evalA (\x -> Identity (p x))

evalA :: (Applicative f) => (a -> f Bool) -> Expr a -> f Bool
evalA p = go
  where
    go (Atom x) = p x
    go (Not e) = not <$> go e
    go (Or e1 e2) = liftA2 (||) (go e1) (go e2)
    go (And e1 e2) = liftA2 (&&) (go e1) (go e2)

-------------------------------------------------------------------------------
--                                   Lexing                                  --
-------------------------------------------------------------------------------

data ExprToken
  = -- Values
    TokenString Text
  | -- Exprs
    TokenExprAnd
  | TokenExprNot
  | TokenExprOr
  | TokenExprXor
  | -- Parens
    TokenParenL
  | TokenParenR
  deriving stock (Eq, Ord, Show)

instance Display ExprToken where
  displayBuilder = \case
    TokenString s -> displayBuilder s
    TokenExprAnd -> "and"
    TokenExprNot -> "not"
    TokenExprOr -> "or"
    TokenExprXor -> "xor"
    TokenParenL -> "("
    TokenParenR -> ")"

-- NOTE: In order to use megaparsec's great errorBundlePretty, we need our
-- token to implement the following two classes. Our implementation is quite
-- basic, essentially copying the upstream strategy from the source, making
-- changes where needed to satisfy the types.
--
-- It is thus quite possible that the current implementation could look
-- weird in some scenarios, and would benefit from a more sophisticated
-- approach. If so, consider implementing the example from the tutorial:
--
-- https://markkarpov.com/tutorial/megaparsec.html#parse-errors

instance TraversableStream (List ExprToken) where
  reachOffset o pst = (Just str, pst')
    where
      pst' :: PosState (List ExprToken)
      pst' =
        PosState
          { pstateInput = post,
            pstateOffset = max o pst.pstateOffset,
            pstateSourcePos = pst.pstateSourcePos,
            pstateTabWidth = pst.pstateTabWidth,
            pstateLinePrefix = pst.pstateLinePrefix
          }

      str =
        unpackText
          . T.intercalate " "
          $ display
          <$> post

      (_, post) = L.splitAt (o - pst.pstateOffset) pst.pstateInput

instance VisualStream (List ExprToken) where
  showTokens _ = L.intercalate " " . toList . fmap showTok
    where
      showTok =
        unpackText
          . (\t -> "'" <> t <> "'")
          . display

  -- Controls the number of carrots e.g.
  --
  --     1 | or distance > 4 or date < 2024
  --       | ^^
  --
  -- Need to account for the fact that our lengths are custom i.e. based on
  -- the string (display) encoding. We cannot reuse the showTokens function
  -- above since it adds single quotes, which are not part of the rendering
  -- here.
  tokensLength _ =
    length
      . L.intercalate " "
      . toList
      . fmap (unpackText . display)

exprLexer :: MParser (List ExprToken)
exprLexer =
  some
    $ asum
      [ TokenExprAnd <$ P.string "and",
        TokenExprAnd <$ P.string "&&",
        TokenExprAnd <$ P.char '∧',
        TokenExprNot <$ P.string "not",
        TokenExprNot <$ P.char '!',
        TokenExprNot <$ P.char '¬',
        TokenExprOr <$ P.string "or",
        TokenExprOr <$ P.string "||",
        TokenExprOr <$ P.char '∨',
        TokenExprXor <$ P.string "xor",
        TokenExprXor <$ P.char '⊕',
        TokenParenL <$ P.symbol "(",
        TokenParenR <$ P.symbol ")",
        TokenString <$> P.allExcept "()"
      ]

-------------------------------------------------------------------------------
--                                  Parsing                                  --
-------------------------------------------------------------------------------

exprParser :: Parsec Void (List ExprToken) (Expr Text)
exprParser = Combs.makeExprParser term table <?> "expression"

term :: Parsec Void (List ExprToken) (Expr Text)
term = parens <|> parseAtom
  where
    parens = MP.single TokenParenL *> exprParser <* MP.single TokenParenR

    parseAtom :: Parsec Void (List ExprToken) (Expr Text)
    parseAtom = do
      -- Because our lexer breaks up whitespace, we may end up with
      -- several TokenString tokens between keywords e.g.
      --
      --     "label foo and distance >= 5 km"
      --
      -- will be lexed as
      --
      --     [ TokenString "label",
      --       TokenString "foo",
      --       TokenExprAnd,
      --       TokenString "distance",
      --       TokenString ">=",
      --       TokenString "5",
      --       TokenString "km"
      --     ]
      --
      -- Our second stage of parsing requires these "broken" strings to be
      -- intact, i.e. we want "label foo" and "distance >= 5 km".
      -- Hence when parsing an atom we take all consecutive TokenStrings and
      -- concat them back together, separating by a single whitespace.
      ts <-
        MP.takeWhile1P
          (Just "atom")
          (\case TokenString _ -> True; _ -> False)
      pure $ Atom $ T.intercalate " " $ fmap concatStrs ts
      where
        concatStrs (TokenString t) = t
        concatStrs _ = ""

table :: List (List (Operator (Parsec Void (List ExprToken)) (Expr Text)))
table =
  [ [ P.prefix [TokenExprNot] Not
    ],
    [ P.binary [TokenExprAnd] And
    ],
    [ P.binary [TokenExprOr] Or,
      P.binary [TokenExprXor] (\x y -> Or (And x (Not y)) (And (Not x) y))
    ]
  ]

lexParse ::
  forall a.
  (Parser a) =>
  Text ->
  ResultDefault (Expr a)
lexParse txt =
  case P.parseAllWith exprLexer txt of
    Err err -> fail $ fmtErr Nothing Nothing err
    Ok ts -> case P.parseWith exprParser ts of
      Err err2 -> fail $ fmtErr (Just ts) Nothing err2
      Ok expr -> case traverse P.parseAll expr of
        Err err3 -> fail $ fmtErr (Just ts) (Just expr) err3
        Ok x -> pure x
  where
    fmtErr :: Maybe (List ExprToken) -> Maybe (Expr Text) -> String -> String
    fmtErr mTokens mExprText err =
      unpackText
        $ mconcat
          [ "Text: '",
            txt,
            "'",
            mFmt "Tokens" mTokens,
            mFmt "Expr Text" mExprText,
            "\n\nError: ",
            packText err
          ]

    mFmt _ Nothing = ""
    mFmt hdr (Just x) = "\n\n" <> hdr <> ": " <> display x
