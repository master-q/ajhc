module Jhc.Grin.Show where

import Jhc.Doc.Pretty
import Jhc.Atom
import {-# SOURCE #-} Jhc.Grin.Grin

prettyFun :: (Atom.Atom,Grin.Grin.Lam) -> Doc.Pretty.Doc
prettyExp :: Doc.Pretty.Doc -> Grin.Grin.Exp -> Doc.Pretty.Doc
prettyVal :: Grin.Grin.Val -> Doc.Pretty.Doc
