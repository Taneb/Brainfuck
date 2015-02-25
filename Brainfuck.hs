module Main where

import Control.Arrow (first)

type Tape = ([Integer], [Integer])

interpret1 :: String -> Tape -> IO (String, Tape)
interpret1 ('+':s) (l:ls, rs) = return (s, (l+1:ls, rs))
interpret1 ('-':s) (l:ls, rs) = return (s, (l-1:ls, rs))
interpret1 ('<':s) (ls, r:rs) = return (s, (r:ls, rs))
interpret1 ('>':s) (l:ls, rs) = return (s, (ls, l:rs))
interpret1 ('.':s) t@(l:_, _) =
  putChar (toEnum . fromInteger $ l) >> return (s, t)
interpret1 (',':s) (_:ls, rs) =
  getChar >>= \l -> return (s, ((toInteger . fromEnum $ l) : ls, rs))
interpret1 ('[':s) t0 = go t0
  where
    (sn, sr) = findEndOfLoop 0 s
      where
        findEndOfLoop n ('[':p) = first ('[':) $ findEndOfLoop (n+1) p
        findEndOfLoop 0 (']':p) = ("", p)
        findEndOfLoop n (']':p) = first (']':) $ findEndOfLoop (n-1) p
        findEndOfLoop n (c  :p) = first (c  :) $ findEndOfLoop  n    p
    go t@(0:_,_) = return (sr, t)
    go t= interpret sn t >>= go
interpret1 (_ :s) t = return (s, t)

interpret :: String -> Tape -> IO Tape
interpret "" t = return t
interpret s t = interpret1 s t >>= uncurry interpret

main :: IO ()
main = do
  s <- getLine
  _ <- interpret s (repeat 0, repeat 0)
  return ()
