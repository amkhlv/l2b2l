module BystroConf where

import           Text.XML.HXT.Core
import           Text.XML.HXT.Arrow.Pickle
import           Data.Maybe
import           Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

data ScribblingConf = ScribblingConf
  { name :: String
  , multipage :: Bool
  , dest :: Maybe String
  }

instance XmlPickler ScribblingConf where
  xpickle = xpScribblingConf

xpScribblingConf :: PU ScribblingConf
xpScribblingConf =
  xpElem "scribbling" $
  xpWrap (  \ (na,mu,ds) -> ScribblingConf (trim na) (case mu of {Just _ -> True;  Nothing -> False}) (trim <$> ds)
          , \ s -> (name s, if multipage s then Just "" else Nothing, dest s)) $
  xpTriple (xpElem "name" xpText) (xpOption (xpElem "multipage" xpText0)) (xpOption (xpElem "dest" xpText))

getScribblingConfs' :: IO [Maybe ScribblingConf]
getScribblingConfs' = runX $ readDocument [withRemoveWS yes] "bystrotex.xml"  >>>
  getChildren >>>
  getChildren >>>
  getChildren >>^
  unpickleDoc xpScribblingConf

getScribblingConfs :: IO [ ScribblingConf ]
getScribblingConfs = catMaybes <$> getScribblingConfs'



