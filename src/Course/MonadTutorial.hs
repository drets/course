{-# LANGUAGE NoImplicitPrelude #-}

module Course.MonadTutorial where

import Control.Category(Category((.)))
import Control.Monad(Monad(..), (=<<))
import Data.Eq(Eq)
import Data.Foldable(foldr)
import Data.Functor(Functor(fmap))
import Data.Int(Int)
import Data.String(IsString(fromString))
import Prelude(Show)
import System.IO(IO)

{-

--------------------------------------------------------------------------------
WARNING: DO NOT PROCEED
-----------------------

It is strongly advised that pre-requisite exercises have been covered prior to
utilising this tutorial. Refusing this advice increases likelihood of a crash
and burn result.

Please complete the following exercises before proceeding:
* Course/Functor
* Course/Applicative
--------------------------------------------------------------------------------

In this source file, you will find a recurring pattern:

* A data structure definition.
* A function named @bind<name>@ for that data structure. The bind function will
  follow a specific pattern in its type:

  @(a -> f b) -> f a -> f b@

* A function named @pure<name>@ for that data structure. The pure function will
  follow a specific pattern in its type:

  @a -> f a@

* A function named @sequence<name>@ for that data structure. The sequence
  function will follow a specific pattern in its type:

  @[f a] -> f [a]

Note that the sequence functions are written in terms of the bind and pure
functions for that data type. The goal is to first acknowledge the repeating
code in the sequence functions, and then construct a plan to refactor out the
similarities. Ultimately, there should be only a single sequence function that is
written in terms of "things that have bind and pure functions."

A type-class denoting "things that have bind and pure functions" is provided. It
is named @BindAndPure@.

Examine the existing data structures, their implementations of bind and pure,
then implement a single sequence function that generalises all the specific
sequence functions.

The data structures given are:
* Id
* Optional
* IntReader
* Reader
* IntState
* State
* Or
* ListFree
* IntReaderFree
* ReaderFree
* Free
* IO

-}

data Id a =
  Id a
  deriving (Eq, Show)

bindId ::
  (a -> Id b)
  -> Id a
  -> Id b
bindId f (Id a) =
  f a

pureId ::
  a
  -> Id a
pureId =
  Id

instance BindAndPure Id where
  pure = pureId
  bind = bindId

----

data Optional a =
  Empty
  | Full a
  deriving (Eq, Show)

bindOptional ::
  (a -> Optional b)
  -> Optional a
  -> Optional b
bindOptional _ Empty =
  Empty
bindOptional f (Full a) =
  f a

pureOptional ::
  a
  -> Optional a
pureOptional =
  Full

instance BindAndPure Optional where
  pure = pureOptional
  bind = bindOptional

----

data IntReader a =
  IntReader (Int -> a)

bindIntReader ::
  (a -> IntReader b)
  -> IntReader a
  -> IntReader b
bindIntReader f (IntReader g) =
  IntReader (\x -> let IntReader r = f (g x) in r x)

pureIntReader ::
  a
  -> IntReader a
pureIntReader =
  IntReader . return

instance BindAndPure IntReader where
  pure = pureIntReader
  bind = bindIntReader

----

data Reader r a =
  Reader (r -> a)

bindReader ::
  (a -> Reader r b)
  -> Reader r a
  -> Reader r b
bindReader f (Reader g) =
  Reader (\x -> let Reader r = f (g x) in r x)

pureReader ::
  a
  -> Reader r a
pureReader =
  Reader . return

instance BindAndPure (Reader r) where
  pure = pureReader
  bind = bindReader

----

data IntState a =
  IntState (Int -> (a, Int))

bindIntState ::
  (a -> IntState b)
  -> IntState a
  -> IntState b
bindIntState f (IntState g) =
  IntState (\i ->
    let (a, j) = g i
        IntState h = f a
    in h j)

pureIntState ::
  a
  -> IntState a
pureIntState a =
  IntState (\i -> (a, i))

instance BindAndPure IntState where
  pure = pureIntState
  bind = bindIntState

----

data State s a =
  State (s -> (a, s))

bindState ::
  (a -> State s b)
  -> State s a
  -> State s b
bindState f (State g) =
  State (\s ->
    let (a, t) = g s
        State h = f a
    in h t)

pureState ::
  a
  -> State s a
pureState a =
  State (\s -> (a, s))

instance BindAndPure (State s) where
  pure = pureState
  bind = bindState

----

data Or t a =
  This t
  | That a
  deriving (Eq, Show)

bindOr ::
  (a -> Or t b)
  -> Or t a
  -> Or t b
bindOr _ (This t) =
  This t
bindOr f (That a) =
  f a

pureOr ::
  a
  -> Or t a
pureOr =
  That

instance BindAndPure (Or t) where
  pure = pureOr
  bind = bindOr

----

data ListFree a =
  ListDone a
  | ListMore [ListFree a]
  deriving (Eq, Show)

bindListFree ::
  (a -> ListFree b)
  -> ListFree a
  -> ListFree b
bindListFree f (ListDone a) =
  f a
bindListFree f (ListMore r) =
  ListMore (fmap (bindListFree f) r)

pureListFree ::
  a
  -> ListFree a
pureListFree =
  ListDone

instance BindAndPure ListFree where
  pure = pureListFree
  bind = bindListFree

----

data IntReaderFree a =
  IntReaderDone a
  | IntReaderMore [IntReaderFree a]
  deriving (Eq, Show)

bindIntReaderFree ::
  (a -> IntReaderFree b)
  -> IntReaderFree a
  -> IntReaderFree b
bindIntReaderFree f (IntReaderDone a) =
  f a
bindIntReaderFree f (IntReaderMore r) =
  IntReaderMore (fmap (bindIntReaderFree f) r)

pureIntReaderFree ::
  a
  -> IntReaderFree a
pureIntReaderFree =
  IntReaderDone

instance BindAndPure IntReaderFree where
  pure = pureIntReaderFree
  bind = bindIntReaderFree

----

data ReaderFree r a =
  ReaderDone a
  | ReaderMore (Reader r (ReaderFree r a))

bindReaderFree ::
  (a -> ReaderFree r b)
  -> ReaderFree r a
  -> ReaderFree r b
bindReaderFree f (ReaderDone a) =
  f a
bindReaderFree f (ReaderMore (Reader r)) =
  ReaderMore (Reader (bindReaderFree f . r))

pureReaderFree ::
  a
  -> ReaderFree r a
pureReaderFree =
  ReaderDone

instance BindAndPure (ReaderFree r) where
  pure = pureReaderFree
  bind = bindReaderFree

----

data Free f a =
  Done a
  | More (f (Free f a))

bindFree ::
  Functor f =>
  (a -> Free f b)
  -> Free f a
  -> Free f b
bindFree f (Done a) =
  f a
bindFree f (More r) =
  More (fmap (bindFree f) r)

pureFree ::
  a
  -> Free f a
pureFree =
  Done

instance Functor f => BindAndPure (Free f) where
  pure = pureFree
  bind = bindFree

----

-- data IO = …

bindIO ::
  (a -> IO b)
  -> IO a
  -> IO b
bindIO f o =
  f =<< o

pureIO ::
  a
  -> IO a
pureIO =
  return

instance BindAndPure IO where
  pure = pureIO
  bind = bindIO

----

sequence :: BindAndPure f => [f a] -> f [a]
sequence = foldr (\a as ->
             bind (\a' ->
             bind (\as' ->
             pure (a' : as')) as) a)
           (pure [])

class BindAndPure f where
  bind ::
    (a -> f b)
    -> f a
    -> f b
  pure ::
    a
    -> f a
