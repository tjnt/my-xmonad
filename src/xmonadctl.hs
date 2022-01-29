import           Graphics.X11.Xlib        (allocaXEvent, clientMessage,
                                           defaultScreen, internAtom,
                                           openDisplay, rootWindow, sendEvent,
                                           structureNotifyMask, sync)
import           Graphics.X11.Xlib.Extras (currentTime, setClientMessageEvent,
                                           setEventType)
import           System.Environment       (getArgs)

main :: IO ()
main = getArgs >>= mapM_ (sendCommand "XMONAD_COMMAND")

sendCommand :: String -> String -> IO ()
sendCommand addr s = do
  d  <- openDisplay ""
  rw <- rootWindow d $ defaultScreen d
  a <- internAtom d addr False
  m <- internAtom d s False
  allocaXEvent $ \e -> do
      setEventType e clientMessage
      setClientMessageEvent e rw a 32 m currentTime
      sendEvent d rw False structureNotifyMask e
      sync d False
