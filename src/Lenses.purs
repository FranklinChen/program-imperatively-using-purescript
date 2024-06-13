module Game.Lenses where

import Type.Proxy (Proxy(..))
import Data.Lens.Record (prop)
import Data.Lens as Lens
import Game.Data(Game(..), GamePoint(..), GameUnit(..))


score :: forall a b r. Lens.Lens { score :: a | r } { score :: b | r } a b
score = prop (Proxy :: Proxy "score")

units :: forall a b r. Lens.Lens { units :: a | r } { units :: b | r } a b
units = prop (Proxy :: Proxy "units")

boss :: forall a b r. Lens.Lens { "boss" :: a | r } { "boss" :: b | r } a b
boss = prop (Proxy :: Proxy "boss")

_Game :: Lens.Iso' Game
           { "score" :: Int
           , "units" :: Array GameUnit
           , "boss" :: GameUnit
           }
_Game = Lens.iso unwrap Game
  where
    unwrap (Game g) = g

health :: forall a b r. Lens.Lens { "health" :: a | r } { "health" :: b | r } a b
health = prop (Proxy :: Proxy "health")

position :: forall a b r. Lens.Lens { "position" :: a | r } { "position" :: b | r } a b
position = prop (Proxy :: Proxy "position")

_GameUnit :: Lens.Iso' GameUnit
               { "health" :: Int
               , "position" :: GamePoint
               }
_GameUnit = Lens.iso unwrap GameUnit
  where
    unwrap (GameUnit u) = u

x :: forall a b r. Lens.Lens { "x" :: a | r } { "x" :: b | r } a b
x = prop (Proxy :: Proxy "x")

y :: forall a b r. Lens.Lens { "y" :: a | r } { "y" :: b | r } a b
y = prop (Proxy :: Proxy "y")

_GamePoint :: Lens.Iso' GamePoint
                { "x" :: Number
                , "y" :: Number
                }
_GamePoint = Lens.iso unwrap GamePoint
  where
    unwrap (GamePoint p) = p
