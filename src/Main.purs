module Main where

import Control.Bind (discard)
import Control.Monad.Eff (Eff)
import Control.Semigroupoid ((<<<))

import DOM (DOM)

import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor (map)
import Data.Generic (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Unit (Unit)
import Data.Validation.Semigroup (V, unV)

import Flare (UI)

import HomeRunBall (AllCaps, BeginsWith, Capitalized, Contains, EndsWith, VS, checkRules)

import Signal.Channel (CHANNEL)

import Sparkle (class Interactive, Renderable, interactive, sparkle')

import Type.Prelude (RProxy(..))

main :: forall e. Eff (channel :: CHANNEL, dom :: DOM | e) Unit
main = do
  sparkle' "content" "AllCaps" $ fromVS <<< checkRules allCaps
  sparkle' "content" "BeginsWith \"Apple\"" $ fromVS <<< checkRules beginsWithApple
  sparkle' "content" "Capitalized" $ fromVS <<< checkRules capitalized
  sparkle' "content" "Contains \"Foo\"" $ fromVS <<< checkRules containsFoo
  sparkle' "content" "EndsWith \"!\"" $ fromVS <<< checkRules endsWithBang

allCaps :: RProxy (allCaps :: AllCaps)
allCaps = RProxy

beginsWithApple :: RProxy (beginsWithApple :: BeginsWith "Apple")
beginsWithApple = RProxy

capitalized :: RProxy (capitalized :: Capitalized)
capitalized = RProxy

containsFoo :: RProxy (containsFoo :: Contains "foo")
containsFoo = RProxy

endsWithBang :: RProxy (endsWithBang :: EndsWith "!")
endsWithBang = RProxy

-- Boilerplate until more instances exist

newtype VS' a
  = VS' (V (NonEmptyList String) (Const' String (RProxy a)))

derive instance newtypeVS' :: Newtype (VS' a) _

fromVS :: forall a. VS a -> VS' a
fromVS = VS' <<< map (wrap <<< unwrap)

newtype Const' a b
  = Const' a

derive instance newtypeConst' :: Newtype (Const' a b) _
derive instance genericConst' :: (Generic a) => Generic (Const' a b)

instance interactiveVS
  :: ( Generic (Const' String (RProxy a))
     )
  => Interactive (VS' a) where
    interactive :: forall e. UI e (VS' a) -> UI e Renderable
    interactive = interactive <<< map (unV Left Right <<< unwrap)
