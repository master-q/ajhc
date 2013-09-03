module Jhc.DataConstructors where


import Jhc.E.E
import Jhc.Name.Name

data DataTable
followAliases :: DataTable -> E -> E
followAlias :: Monad m => DataTable -> E -> m E
typesCompatable :: Monad m => E -> E -> m ()
updateLit :: DataTable -> Lit e t -> Lit e t
slotTypes :: DataTable -> Name -> E -> [E]
mktBox :: E -> E
