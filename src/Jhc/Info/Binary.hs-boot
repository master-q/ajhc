module Jhc.Info.Binary where

import Jhc.Info.Info
import Data.Binary

putInfo :: Jhc.Info.Info.Info -> Put
getInfo :: Get Jhc.Info.Info.Info
