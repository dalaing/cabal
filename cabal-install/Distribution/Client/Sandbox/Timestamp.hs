-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Sandbox.Timestamp
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Timestamp file handling (for add-source dependencies).
-----------------------------------------------------------------------------

module Distribution.Client.Sandbox.Timestamp (
  AddSourceTimestamp,
  withAddTimestamps,
  withUpdateTimestamps,
  maybeAddCompilerTimestampRecord,
  listModifiedDeps,
  removeTimestamps,

  -- * For testing
  TimestampFileRecord,
  readTimestampFile,
  writeTimestampFile
  ) where

import Control.Monad                                 (filterM, forM, when)
import Data.Char                                     (isSpace)
import Data.List                                     (partition)
import System.Directory                              (renameFile)
import System.FilePath                               ((<.>), (</>))
import qualified Data.Map as M

import Distribution.Compiler                         (CompilerId)
import Distribution.Simple.Utils                     (debug, die', warn)
import Distribution.System                           (Platform)
import Distribution.Text                             (display)
import Distribution.Monad                            (CabalM, liftIO)

import Distribution.Client.SrcDist (allPackageSourceFiles)
import Distribution.Client.Sandbox.Index
  (ListIgnoredBuildTreeRefs (ListIgnored), RefTypesToList(OnlyLinks)
  ,listBuildTreeRefs)
import Distribution.Client.SetupWrapper

import Distribution.Compat.Exception                 (catchIO)
import Distribution.Compat.Time               (ModTime, getCurTime,
                                                      getModTime,
                                                      posixSecondsToModTime)


-- | Timestamp of an add-source dependency.
type AddSourceTimestamp  = (FilePath, ModTime)
-- | Timestamp file record - a string identifying the compiler & platform plus a
-- list of add-source timestamps.
type TimestampFileRecord = (String, [AddSourceTimestamp])

timestampRecordKey :: CompilerId -> Platform -> String
timestampRecordKey compId platform = display platform ++ "-" ++ display compId

-- | The 'add-source-timestamps' file keeps the timestamps of all add-source
-- dependencies. It is initially populated by 'sandbox add-source' and kept
-- current by 'reinstallAddSourceDeps' and 'configure -w'. The user can install
-- add-source deps manually with 'cabal install' after having edited them, so we
-- can err on the side of caution sometimes.
-- FIXME: We should keep this info in the index file, together with build tree
-- refs.
timestampFileName :: FilePath
timestampFileName = "add-source-timestamps"

-- | Read the timestamp file. Exits with error if the timestamp file is
-- corrupted. Returns an empty list if the file doesn't exist.
readTimestampFile :: FilePath -> CabalM [TimestampFileRecord]
readTimestampFile timestampFile = do
  timestampString <- liftIO $ readFile timestampFile `catchIO` \_ -> return "[]"
  case reads timestampString of
    [(version, s)]
      | version == (2::Int) ->
        case reads s of
          [(timestamps, s')] | all isSpace s' -> return timestamps
          _                                   -> dieCorrupted
      | otherwise   -> dieWrongFormat

    -- Old format (timestamps are POSIX seconds). Convert to new format.
    [] ->
      case reads timestampString of
        [(timestamps, s)] | all isSpace s -> do
          let timestamps' = map (\(i, ts) ->
                                  (i, map (\(p, t) ->
                                            (p, posixSecondsToModTime t)) ts))
                            timestamps
          liftIO $ writeTimestampFile timestampFile timestamps'
          return timestamps'
        _ -> dieCorrupted
    _ -> dieCorrupted
  where
    dieWrongFormat    = die' $ wrongFormat ++ deleteAndRecreate
    dieCorrupted      = die' $ corrupted ++ deleteAndRecreate
    wrongFormat       = "The timestamps file is in the wrong format."
    corrupted         = "The timestamps file is corrupted."
    deleteAndRecreate = " Please delete and recreate the sandbox."

-- | Write the timestamp file, atomically.
writeTimestampFile :: FilePath -> [TimestampFileRecord] -> IO ()
writeTimestampFile timestampFile timestamps = do
  writeFile  timestampTmpFile "2\n" -- version
  appendFile timestampTmpFile (show timestamps ++ "\n")
  renameFile timestampTmpFile timestampFile
  where
    timestampTmpFile = timestampFile <.> "tmp"

-- | Read, process and write the timestamp file in one go.
withTimestampFile :: FilePath
                     -> ([TimestampFileRecord] -> CabalM [TimestampFileRecord])
                     -> CabalM ()
withTimestampFile sandboxDir process = do
  let timestampFile = sandboxDir </> timestampFileName
  timestampRecords <- readTimestampFile timestampFile >>= process
  liftIO $ writeTimestampFile timestampFile timestampRecords

-- | Given a list of 'AddSourceTimestamp's, a list of paths to add-source deps
-- we've added and an initial timestamp, add an 'AddSourceTimestamp' to the list
-- for each path. If a timestamp for a given path already exists in the list,
-- update it.
addTimestamps :: ModTime -> [AddSourceTimestamp] -> [FilePath]
                 -> [AddSourceTimestamp]
addTimestamps initial timestamps newPaths =
  [ (p, initial) | p <- newPaths ] ++ oldTimestamps
  where
    (oldTimestamps, _toBeUpdated) =
      partition (\(path, _) -> path `notElem` newPaths) timestamps

-- | Given a list of 'AddSourceTimestamp's, a list of paths to add-source deps
-- we've reinstalled and a new timestamp value, update the timestamp value for
-- the deps in the list. If there are new paths in the list, ignore them.
updateTimestamps :: [AddSourceTimestamp] -> [FilePath] -> ModTime
                    -> [AddSourceTimestamp]
updateTimestamps timestamps pathsToUpdate newTimestamp =
  foldr updateTimestamp [] timestamps
  where
    updateTimestamp t@(path, _oldTimestamp) rest
      | path `elem` pathsToUpdate = (path, newTimestamp) : rest
      | otherwise                 = t : rest

-- | Given a list of 'TimestampFileRecord's and a list of paths to add-source
-- deps we've removed, remove those deps from the list.
removeTimestamps' :: [AddSourceTimestamp] -> [FilePath] -> [AddSourceTimestamp]
removeTimestamps' l pathsToRemove = foldr removeTimestamp [] l
  where
    removeTimestamp t@(path, _oldTimestamp) rest =
      if path `elem` pathsToRemove
      then rest
      else t : rest

-- | If a timestamp record for this compiler doesn't exist, add a new one.
maybeAddCompilerTimestampRecord :: FilePath -> FilePath
                                   -> CompilerId -> Platform
                                   -> CabalM ()
maybeAddCompilerTimestampRecord sandboxDir indexFile
                                compId platform = do
  let key = timestampRecordKey compId platform
  withTimestampFile sandboxDir $ \timestampRecords -> do
    case lookup key timestampRecords of
      Just _  -> return timestampRecords
      Nothing -> do
        buildTreeRefs <- listBuildTreeRefs ListIgnored OnlyLinks
                         indexFile
        now <- liftIO getCurTime
        let timestamps = map (\p -> (p, now)) buildTreeRefs
        return $ (key, timestamps):timestampRecords

-- | Given an IO action that returns a list of build tree refs, add those
-- build tree refs to the timestamps file (for all compilers).
withAddTimestamps :: FilePath -> CabalM [FilePath] -> CabalM ()
withAddTimestamps sandboxDir act = do
  let initialTimestamp = minBound
  withActionOnAllTimestamps (addTimestamps initialTimestamp) sandboxDir act

-- | Given a list of build tree refs, remove those
-- build tree refs from the timestamps file (for all compilers).
removeTimestamps :: FilePath -> [FilePath] -> CabalM ()
removeTimestamps idxFile =
  withActionOnAllTimestamps removeTimestamps' idxFile . return

-- | Given an IO action that returns a list of build tree refs, update the
-- timestamps of the returned build tree refs to the current time (only for the
-- given compiler & platform).
withUpdateTimestamps :: FilePath -> CompilerId -> Platform
                        ->([AddSourceTimestamp] -> CabalM [FilePath])
                        -> CabalM ()
withUpdateTimestamps =
  withActionOnCompilerTimestamps updateTimestamps

-- | Helper for implementing 'withAddTimestamps' and
-- 'withRemoveTimestamps'. Runs a given action on the list of
-- 'AddSourceTimestamp's for all compilers, applies 'f' to the result and then
-- updates the timestamp file. The IO action is run only once.
withActionOnAllTimestamps :: ([AddSourceTimestamp] -> [FilePath]
                              -> [AddSourceTimestamp])
                             -> FilePath
                             -> CabalM [FilePath]
                             -> CabalM ()
withActionOnAllTimestamps f sandboxDir act =
  withTimestampFile sandboxDir $ \timestampRecords -> do
    paths <- act
    return [(key, f timestamps paths) | (key, timestamps) <- timestampRecords]

-- | Helper for implementing 'withUpdateTimestamps'. Runs a given action on the
-- list of 'AddSourceTimestamp's for this compiler, applies 'f' to the result
-- and then updates the timestamp file record. The IO action is run only once.
withActionOnCompilerTimestamps :: ([AddSourceTimestamp]
                                   -> [FilePath] -> ModTime
                                   -> [AddSourceTimestamp])
                                  -> FilePath
                                  -> CompilerId
                                  -> Platform
                                  -> ([AddSourceTimestamp] -> CabalM [FilePath])
                                  -> CabalM ()
withActionOnCompilerTimestamps f sandboxDir compId platform act = do
  let needle = timestampRecordKey compId platform
  withTimestampFile sandboxDir $ \timestampRecords -> do
    timestampRecords' <- forM timestampRecords $ \r@(key, timestamps) ->
      if key == needle
      then do paths <- act timestamps
              now   <- liftIO getCurTime
              return (key, f timestamps paths now)
      else return r
    return timestampRecords'

-- | Has this dependency been modified since we have last looked at it?
isDepModified :: ModTime -> AddSourceTimestamp -> CabalM Bool
isDepModified now (packageDir, timestamp) = do
  debug ("Checking whether the dependency is modified: " ++ packageDir)
  -- TODO: we should properly plumb the correct options through
  -- instead of using defaultSetupScriptOptions
  depSources <- allPackageSourceFiles defaultSetupScriptOptions packageDir
  go depSources

  where
    go []         = return False
    go (dep0:rest) = do
      -- FIXME: What if the clock jumps backwards at any point? For now we only
      -- print a warning.
      let dep = packageDir </> dep0
      modTime <- liftIO $ getModTime dep
      when (modTime > now) $
        warn $ "File '" ++ dep
                         ++ "' has a modification time that is in the future."
      if modTime >= timestamp
        then do
          debug ("Dependency has a modified source file: " ++ dep)
          return True
        else go rest

-- | List all modified dependencies.
listModifiedDeps :: FilePath -> CompilerId -> Platform
                    -> M.Map FilePath a
                       -- ^ The set of all installed add-source deps.
                    -> CabalM [FilePath]
listModifiedDeps sandboxDir compId platform installedDepsMap = do
  timestampRecords <- readTimestampFile (sandboxDir </> timestampFileName)
  let needle        = timestampRecordKey compId platform
  timestamps       <- maybe noTimestampRecord return
                      (lookup needle timestampRecords)
  now <- liftIO getCurTime
  fmap (map fst) . filterM (isDepModified now)
    . filter (\ts -> fst ts `M.member` installedDepsMap)
    $ timestamps

  where
    noTimestampRecord = die' $ "Сouldn't find a timestamp record for the given "
                        ++ "compiler/platform pair. "
                        ++ "Please report this on the Cabal bug tracker: "
                        ++ "https://github.com/haskell/cabal/issues/new ."
