module Jhc.FrontEnd.Tc.Class(simplify,FrontEnd.Class.ClassHierarchy) where

import Jhc.FrontEnd.Class
import Jhc.FrontEnd.Tc.Type


simplify :: ClassHierarchy -> [Pred] -> [Pred]
