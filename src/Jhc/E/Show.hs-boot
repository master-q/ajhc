-- -*- Haskell -*-

module Jhc.E.Show(ePretty,render,prettyE) where

import Jhc.E.E
import Jhc.Doc.DocLike
import Jhc.Doc.Pretty
import Jhc.Doc.PPrint

render :: Doc -> String
prettyE :: E -> String
ePretty :: E -> Doc

instance DocLike d => PPrint d TVr
instance PPrint Doc E
instance PPrint String E
instance PPrint String (Lit E E)
