{-|
Module      : Parsing
Description : Simple Monadic Parsing Library
Maintainer  : dave@chalmers.se

Used in the course Functional Programming GU/Chalmers

-}

module Parsing 
 ( Parser -- exports the type name but not the constructors
  ,parse  
  ,readsP -- behaves as reads :: Read a => [(a,String)]
  ,failure,sat,item,char,digit,
  (+++),(<:>), 
  (>->),(<-<),
  oneOrMore,zeroOrMore,chain
 ) 

{----------------------

Aim: reusable Parser combinators including 
 a new type for the Parser, 
 but no export of the constructor

Changes (v2 2015)
For compatibility with GHC 10.7 Parser 
is now also instance Applicative

Removal: Class Functor, Applicative and Monad provide a number of 
functions that were previously exported explicitly, in particular
(>*>) is available as the bind operation (>>=),
success is return, pmap is fmap.

Additional function:
readsP :: Read a => Parser a  
-- satisfies 
-- parse readsP s == listToMaybe (reads s)

----------------------}
where

import Data.Char
import Data.Maybe(listToMaybe)
-- boilerplate for GHC 10.7 compatibility:
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
 
------------------

-- | The abstract data type representing a Parser
data Parser a = P (String -> Maybe (a,String))

-- | Runs the parser on the given string 
--  to return maybe a thing and a string
parse :: Parser a -> String -> Maybe(a,String)
parse (P f ) s = f s

-- | A parser for anything in the Read class
-- satisfying
-- parse readsP s == listToMaybe (reads s)

readsP :: Read a => Parser a
readsP = P $ listToMaybe . reads 
               
------------------- 
-- | Parser than can never succeed
failure :: Parser a -- always fails
failure    = P $ \s -> 
             Nothing
 
-- | Parser that succeeds without looking at the String
success :: a -> Parser a 
success a  = P $ \s ->
             Just (a,s)

-- | Parse any single character
item  = P $ \s ->
         case s of 
              (c:s') -> Just (c,s')
              ""     -> Nothing


infixr 5 +++
-- | Try the first parser and if it fails try the second
(+++) :: Parser a -> Parser a -> Parser a
p +++ q  = P $ \s ->
             case parse p s of
                  Nothing -> parse q s
                  r       -> r


-- (p >*> f) parse using p to produce a. 
-- Then parse using f a 

infixl 1 >*>

(>*>) :: Parser a -> (a -> Parser b) -> Parser b
p >*> f  = P $ \s -> 
            case parse p s of 
                 Just (a,s') -> parse (f a) s'
                 Nothing     -> Nothing
-----------------------------------------------
-- Parsers below do not depend on the internal 
-- representation of Parser

-- | parse a single character satisfying property p
sat :: (Char -> Bool) -> Parser Char
sat p  = item >*> \a -> if p a then success a else failure

-- | parse a digit character
digit :: Parser Char
digit = sat isDigit

-- | Parse a specific character
char c = sat (==c)

-- example: parse any lowercase letter 
-- followed by its uppercase equivalent aA or bB etc.
ex1 = sat isAsciiLower >*> char . toUpper

-- pmap modifies the result of a parser

-- | Parse with first, ignore the result 
--   and parse with the second.
-- Equivalent to (>>)
(>->) :: Parser a -> Parser b -> Parser b
p >-> q = p >*> \_ -> q

-- | Parse with first, then the second, 
--  returning the result of the first
(<-<) :: Parser b -> Parser a -> Parser b
p <-< q = p >*> \a -> q >-> success a

-- | Parse a thing, then parse a list of things, and 
-- return the first thing followed by the list of things
(<:>):: Parser a -> Parser [a] -> Parser [a]
p <:> q = p >*> \a -> fmap (a:) q 

-- | Parse zero or more things 
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p +++ success []

-- | Parse one or more things 
oneOrMore :: Parser a -> Parser [a]
oneOrMore  p = p <:> zeroOrMore p

-- | Parse a list of as, separated by bs
chain :: Parser a -> Parser b -> Parser [a]
chain p q = p <:> zeroOrMore (q >-> p)

-- example: comma separated digits "1,2,3"

-- Standard definition for Functor and Applicative for 
-- GHC 10.7 compatibility 
instance Functor Parser where
    fmap = liftM
instance Applicative Parser where
    pure  = success
    (<*>) = ap
instance Monad Parser where
    (>>=)  = (>*>)
    return = pure 

