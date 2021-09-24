module Hooks.AvoidDocksFloat
    ( doAvoidDocksFloat
    , doAvoidDocksRectFloat
    , doAvoidDocksFullFloat
    ) where

import qualified Data.Set                   as S
import           XMonad                     (ManageHook, X, ask, doF,
                                             floatLocation, gets, liftX,
                                             scaleRationalRect, screenRect,
                                             windowset)
import           XMonad.Hooks.ManageDocks   (calcGap)
import           XMonad.Hooks.ManageHelpers (doRectFloat)
import           XMonad.Layout.Gaps         (Direction2D (..))
import qualified XMonad.StackSet            as W
import           XMonad.Util.Rectangle      (toRatio)

doAvoidDocksFloat :: ManageHook
doAvoidDocksFloat = ask >>= \w ->
    liftX (floatLocation w >>= calcRect . snd) >>= doF . W.float w

doAvoidDocksRectFloat :: W.RationalRect -> ManageHook
doAvoidDocksRectFloat r = liftX (calcRect r) >>= doRectFloat

doAvoidDocksFullFloat :: ManageHook
doAvoidDocksFullFloat = doAvoidDocksRectFloat (W.RationalRect 0 0 1 1)

calcRect :: W.RationalRect -> X W.RationalRect
calcRect r = do
    screen <- W.current <$> gets windowset
    let sr = screenRect $ W.screenDetail screen
        r' = scaleRationalRect sr r
    gd <- calcGap (S.fromList [U,D,L,R])
    return $ toRatio (gd r') sr
