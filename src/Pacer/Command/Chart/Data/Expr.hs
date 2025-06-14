module Pacer.Command.Chart.Data.Expr
  ( -- * Expressions
    Expr (..),
    FilterExpr,
    eval,

    -- ** Parsing
    ExprToken (..),
    exprLexer,
    exprParser,

    -- * Misc
    guardActivityType,
    guardMActivityType,
    lexParse,
  )
where

import Control.Monad.Combinators.Expr (Operator)
import Control.Monad.Combinators.Expr qualified as Combs
import Data.Foldable qualified as F
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.List qualified as L
import Data.Text qualified as T
import Pacer.Class.Parser (MParser, Parser)
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Activity.ActivityType (ActivityType)
import Pacer.Command.Chart.Data.Expr.Filter (FilterType (FilterType))
import Pacer.Command.Chart.Data.Expr.Set qualified as Set
import Pacer.Prelude
import Pacer.Utils.Json (FromJSON (parseJSON))
import Pacer.Utils.Json qualified as Json
import Text.Megaparsec
  ( Parsec,
    PosState (PosState),
    TraversableStream (reachOffset),
    VisualStream,
    (<?>),
  )
import Text.Megaparsec qualified as MP

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
        Not e -> "not " <> goP e
        Or e1 e2 ->
          mconcat
            [ goP e1,
              " or ",
              goP e2
            ]
        And e1 e2 ->
          mconcat
            [ goP e1,
              " and ",
              goP e2
            ]
      goP e =
        if isAtom e
          then go e
          else "(" <> go e <> ")"

-- | Alias for a filter expression.
type FilterExpr a = Expr (FilterType a)

instance (Parser a) => FromJSON (Expr a) where
  parseJSON = Json.withText "Expr" (failErr . lexParse)

eval :: (a -> Bool) -> Expr a -> Bool
eval p = runIdentity . evalA (Identity . p)

evalA :: (Applicative f) => (a -> f Bool) -> Expr a -> f Bool
evalA p = go
  where
    go (Atom x) = p x
    go (Not e) = not <$> go e
    go (Or e1 e2) = liftA2 (||) (go e1) (go e2)
    go (And e1 e2) = liftA2 (&&) (go e1) (go e2)

isAtom :: Expr a -> Bool
isAtom (Atom _) = True
isAtom _ = False

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

-- See NOTE: [Operators]
--
-- Notice that instead of unicode output, we have plain language.
-- This is arguably clearer because not everyone is familiar with logical
-- operators.

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
            pstateOffset = max o $ pst ^. #pstateOffset,
            pstateSourcePos = pst ^. #pstateSourcePos,
            pstateTabWidth = pst ^. #pstateTabWidth,
            pstateLinePrefix = pst ^. #pstateLinePrefix
          }

      str =
        unpackText
          . T.intercalate " "
          $ display
          <$> post

      (_, post) = L.splitAt (o - pst ^. #pstateOffset) (pst ^. #pstateInput)

instance VisualStream (List ExprToken) where
  showTokens _ = L.unwords . toList . fmap showTok
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
      . L.unwords
      . toList
      . fmap (unpackText . display)

-- See NOTE: [Operators]
--
-- Notice the expressions are a bit of an exception wrt 3, because we have e.g.
-- 'and', '&&', '∧'. We have our two ascii/unicode operators, but also a
-- plain-language variant.

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
      --     "label = foo and distance >= 5 km"
      --
      -- will be lexed as
      --
      --     [ TokenString "label",
      --       TokenString "=",
      --       TokenString "foo",
      --       TokenExprAnd,
      --       TokenString "distance",
      --       TokenString ">=",
      --       TokenString "5",
      --       TokenString "km"
      --     ]
      --
      -- Our second stage of parsing requires these "broken" strings to be
      -- intact, i.e. we want "label = foo" and "distance >= 5 km".
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
lexParse txt = do
  ts <- first (fmtErr Nothing Nothing) $ P.parseAllWith exprLexer txt
  expr <- first (fmtErr (Just ts) Nothing) $ P.parseWith exprParser ts
  first (fmtErr (Just ts) (Just expr)) $ traverse P.parseAll expr
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

-- | Like 'guardActivityType', except the type may not exist. This will be
-- filtered out, if any type filters are given.
guardMActivityType ::
  (Applicative f) =>
  -- | Filters to run on the type.
  List (Expr (FilterType a)) ->
  -- | Possible Type.
  Maybe ActivityType ->
  -- | Action.
  f b ->
  f (Maybe b)
guardMActivityType globalFilters mAType m = do
  if F.all (eval filterType) globalFilters
    then Just <$> m
    else
      pure Nothing
  where
    filterType (FilterType fe) = case mAType of
      Nothing -> False
      Just atype -> Set.applyFilterElem atype fe
    filterType _ = True

-- | Runs the given action iff the type is not filtered out.
guardActivityType ::
  (Applicative f) =>
  -- | Filters to run on the type.
  List (Expr (FilterType a)) ->
  -- | Type.
  ActivityType ->
  -- | Action.
  f b ->
  f (Maybe b)
guardActivityType globalFilters atype =
  guardMActivityType globalFilters (Just atype)
