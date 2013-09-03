{-# LANGUAGE TemplateHaskell #-}
module Jhc.Ho.Type where

import Data.Monoid
import qualified Data.ByteString as BS
import Data.Map
import Data.DeriveTH

import Data.Version
import Jhc.DataConstructors(DataTable)
import Jhc.E.Rules(Rules)
import Jhc.E.Type
import Jhc.E.TypeCheck()
import Jhc.FrontEnd.Class(ClassHierarchy)
import Jhc.FrontEnd.Infix(FixityMap)
import Jhc.FrontEnd.KindInfer(KindEnv)
import Jhc.FrontEnd.Rename(FieldMap())
import Jhc.FrontEnd.SrcLoc(SrcLoc)
import Jhc.FrontEnd.Tc.Type(Type())
import Jhc.FrontEnd.TypeSynonyms(TypeSynonyms)
import Jhc.Name.Id
import Jhc.Name.Name(Name,Module)
import Jhc.PackedString
import Jhc.Util.SetLike
import Jhc.Support.CFF
import Jhc.Support.MapBinaryInstance()
import Jhc.Support.MD5

cff_magic = chunkType "JHC"
cff_link  = chunkType "LINK"
cff_libr  = chunkType "LIBR"
cff_jhdr  = chunkType "JHDR"
cff_core  = chunkType "CORE"
cff_defs  = chunkType "DEFS"
cff_lcor  = chunkType "LCOR"
cff_ldef  = chunkType "LDEF"
cff_idep  = chunkType "IDEP"
cff_file  = chunkType "FILE"

-- | A SourceHash is the hash of a specific file, it is associated with a
-- specific 'Module' that said file implements.
type SourceHash = Hash
-- | HoHash is a unique identifier for a ho file or library.
type HoHash     = Hash

-- | while a 'Module' is a single Module associated with a single haskell source
-- file, a 'ModuleGroup' identifies a group of mutually recursive modules.
-- Generally it is chosen from among the Modules making up the group, but the
-- specific choice has no other meaning. We could use the HoHash, but for readability
-- reasons when debugging it makes more sense to choose an arbitrary Module.
type ModuleGroup = Module

-- | the collected information that is passed around
-- this is not stored in any file, but is what is collected from the ho files.
data CollectedHo = CollectedHo {
    -- | this is a list of external names that are valid but that we may not know
    -- anything else about it is used to recognize invalid ids.
    choExternalNames :: IdSet,
    -- | these are the functions in Comb form.
    choCombinators  :: IdMap Comb,
    -- | these are rules that may need to be retroactively applied to other
    -- modules
    choOrphanRules :: Rules,
    -- | the hos
    choHoMap :: Map ModuleGroup Ho,
    -- | libraries depended on
    choLibDeps :: Map PackedString HoHash,
    -- | these are caches of pre-computed values
    choHo :: Ho, -- ^ cache of combined and renamed ho
    choVarMap :: IdMap (Maybe E) -- ^ cache of variable substitution map
    }

-- | The header contains basic information about the file, it should be enough to determine whether
-- we can discard the file right away or consider it further.
data HoHeader = HoHeader {
    -- | the version of the file format. it comes first so we don't try to read data that may be in a different format.
    hohVersion  :: Int,
    -- | my sha1 id
    hohHash     :: HoHash,
    -- | the human readable name, either the ModuleGroup or the library name and version.
    hohName     :: Either ModuleGroup (PackedString,Version),
    -- | library dependencies
    hohLibDeps  :: [(PackedString,HoHash)],
    -- | arch dependencies, these say whether the file is specialized for a
    -- given arch.
    hohArchDeps :: [(PackedString,PackedString)]
    }

-- | These are the dependencies needed to check if a ho file is up to date.  it
-- only appears in ho files as hl files do not have source code to check
-- against or depend on anything but other libraries.
data HoIDeps = HoIDeps {
    -- | modules depended on indexed by a hash of the source.
    hoIDeps :: Map SourceHash (Module,[(Module,SrcLoc)]),
    -- | Haskell Source files depended on
    hoDepends    :: [(Module,SourceHash)],
    -- | Other objects depended on to be considered up to date.
    hoModDepends :: [HoHash],
    -- | library module groups needed
    hoModuleGroupNeeds :: [ModuleGroup]
    }

data HoLib = HoLib {
    -- | arbitrary metainformation such as library author, web site, etc.
    hoModuleMap  :: Map Module ModuleGroup,
    hoReexports  :: Map Module Module,
    hoModuleDeps :: Map ModuleGroup [ModuleGroup],
    hoMetaInfo   :: [(PackedString,PackedString)]
    }

data Library = Library {
    libHoHeader :: HoHeader,
    libHoLib :: HoLib,
    libTcMap :: (Map ModuleGroup HoTcInfo),
    libBuildMap :: (Map ModuleGroup HoBuild),
    libExtraFiles :: [ExtraFile],
    libFileName :: FilePath
    }

instance Show Library where
    showsPrec n lib = showsPrec n (hohHash $ libHoHeader lib)

-- | data only needed for type checking.
data HoTcInfo = HoTcInfo {
    hoExports :: Map Module [Name],
    hoDefs :: Map Name (SrcLoc,[Name]),
    hoAssumps :: Map Name Type,        -- ^ used for typechecking
    hoFixities :: FixityMap,
    hoKinds :: KindEnv,                    -- ^ used for typechecking
    hoTypeSynonyms :: TypeSynonyms,
    hoClassHierarchy :: ClassHierarchy,
    hoFieldMap :: FieldMap
    }

data HoBuild = HoBuild {
    -- | Filled in by E generation
    hoDataTable :: DataTable,
    hoEs :: [(TVr,E)],
    hoRules :: Rules
    }

data Ho = Ho {
    hoModuleGroup :: ModuleGroup,
    hoTcInfo :: HoTcInfo,
    hoBuild :: HoBuild
    }

instance Monoid Ho where
    mempty = Ho (error "unknown module group") mempty mempty
    mappend ha hb = Ho (hoModuleGroup ha) (hoTcInfo ha `mappend` hoTcInfo hb) (hoBuild ha `mappend` hoBuild hb)

data ExtraFile = ExtraFile {
    extraFileName :: PackedString,
    extraFileData :: BS.ByteString
    }

{-
instance Monoid Ho where
    mempty = Ho mempty mempty
    mappend a b = Ho {
        hoTcInfo = hoTcInfo a `mappend` hoTcInfo b,
        hoBuild = hoBuild a `mappend` hoBuild b
    }

instance Monoid HoTcInfo where
    mempty = HoTcInfo mempty mempty
    mappend a b = HoTcInfo {
        hoExports = hoExports a `mappend` hoExports b,
        hoDefs = hoDefs a `mappend` hoDefs b
    }

instance Monoid HoBuild where
    mempty = HoBuild mempty mempty mempty mempty mempty mempty mempty mempty
    mappend a b = HoBuild {
        hoAssumps = hoAssumps a `mappend` hoAssumps b,
        hoFixities = hoFixities a `mappend` hoFixities b,
        hoKinds = hoKinds a `mappend` hoKinds b,
        hoClassHierarchy = hoClassHierarchy a `mappend` hoClassHierarchy b,
        hoTypeSynonyms = hoTypeSynonyms a `mappend` hoTypeSynonyms b,
        hoDataTable = hoDataTable a `mappend` hoDataTable b,
        hoEs = hoEs a `mappend` hoEs b,
        hoRules = hoRules a `mappend` hoRules b
    }

 -}

$(derive makeUpdate ''CollectedHo)
$(derive makeUpdate ''HoTcInfo)
$(derive makeMonoid ''HoTcInfo)
$(derive makeUpdate ''HoBuild)
$(derive makeMonoid ''HoBuild)
$(derive makeUpdate ''Ho)
