-- The code is mostly ripped from
-- https://github.com/ekmett/lens/blob/697582fb9a980f273dbf8496253c5bbefedd0a8b/Setup.lhs
import Data.List ( nub )
import Data.Version ( showVersion )
import Distribution.Package ( PackageName(PackageName), Package, PackageId, InstalledPackageId, packageVersion, packageName )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..) )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose, copyFiles )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), Flag(..), fromFlag, HaddockFlags(haddockDistPref))
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps) )
import Distribution.Text ( display )
import Distribution.Verbosity ( Verbosity, normal )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    withTestLBI pkg lbi $ \suite suitecfg -> do
      rewriteFile (dir </> "Build_" ++ testName suite ++ ".hs") $ unlines
        [ "module Build_" ++ testName suite ++ " where"
        , "import Prelude"
        , "deps :: [String]"
        , "deps = " ++ (show $ formatdeps (testDeps libcfg suitecfg))
        ]
  where
    formatdeps = map (formatone . snd)
    formatone p = case packageName p of
      PackageName n -> n ++ "-" ++ showVersion (packageVersion p)

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

