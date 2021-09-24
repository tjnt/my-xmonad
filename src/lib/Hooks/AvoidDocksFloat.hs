module Hooks.AvoidDocksFloat
    ( doFloat
    , doRectFloat
    , doFullFloat
    ) where

import qualified Data.Set                 as S
import           XMonad                   (ManageHook, X, ask, doF,
                                           floatLocation, gets, liftX,
                                           scaleRationalRect, screenRect,
                                           windowset)
import           XMonad.Hooks.ManageDocks (calcGap)
import           XMonad.Layout.Gaps       (Direction2D (..))
import qualified XMonad.StackSet          as W
import           XMonad.Util.Rectangle    (toRatio)

doFloat :: ManageHook
doFloat = ask >>= \w ->
    liftX (floatLocation w >>= calcRect . snd) >>= doF . W.float w

doRectFloat :: W.RationalRect -> ManageHook
doRectFloat r = ask >>= \w ->
    liftX (calcRect r) >>= doF . W.float w

doFullFloat :: ManageHook
doFullFloat = doRectFloat (W.RationalRect 0 0 1 1)

calcRect :: W.RationalRect -> X W.RationalRect
calcRect r = do
    screen <- W.current <$> gets windowset
    let sr = screenRect $ W.screenDetail screen
        r' = scaleRationalRect sr r
    gd <- calcGap (S.fromList [U,D,L,R])
    return $ toRatio (gd r') sr
