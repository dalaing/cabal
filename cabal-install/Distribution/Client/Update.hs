-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Update
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.Update
    ( update
    ) where

import Distribution.Simple.Setup
         ( fromFlag )
import Distribution.Client.Compat.Directory
         ( setModificationTime )
import Distribution.Client.Types
         ( Repo(..), RemoteRepo(..), maybeRepoRemote )
import Distribution.Client.HttpUtils
         ( DownloadResult(..) )
import Distribution.Client.FetchUtils
         ( downloadIndex )
import Distribution.Client.IndexUtils.Timestamp
import Distribution.Client.IndexUtils
         ( updateRepoIndexCache, Index(..), writeIndexTimestamp
         , currentIndexTimestamp, indexBaseName )
import Distribution.Client.JobControl
         ( newParallelJobControl, spawnJob, collectJob )
import Distribution.Client.Setup
         ( RepoContext(..), UpdateFlags(..) )
import Distribution.Text
         ( display )
import Distribution.Verbosity
import Distribution.Monad

import Distribution.Simple.Utils
         ( writeFileAtomic, warn, notice, noticeNoWrap )

import qualified Data.ByteString.Lazy       as BS
import Distribution.Client.GZipUtils (maybeDecompress)
import System.FilePath ((<.>), dropExtension)
import Data.Maybe (mapMaybe)
import Data.Time (getCurrentTime)
import Control.Monad

import qualified Hackage.Security.Client as Sec

-- | 'update' downloads the package list from all known servers
update :: UpdateFlags -> RepoContext -> CabalM ()
update _ repoCtxt | null (repoContextRepos repoCtxt) = do
  warn $ "No remote package servers have been specified. Usually "
                ++ "you would have one specified in the config file."
update updateFlags repoCtxt = do
  let repos       = repoContextRepos repoCtxt
      remoteRepos = mapMaybe maybeRepoRemote repos
  case remoteRepos of
    [] -> return ()
    [remoteRepo] ->
        notice $ "Downloading the latest package list from "
                        ++ remoteRepoName remoteRepo
    _ -> notice . unlines
            $ "Downloading the latest package lists from: "
            : map (("- " ++) . remoteRepoName) remoteRepos
  jobCtrl <- liftIO $ newParallelJobControl (length repos)
  runCabalMInIO $ \liftC -> mapM_ (spawnJob jobCtrl . liftC . updateRepo updateFlags repoCtxt) repos
  liftIO $ mapM_ (\_ -> collectJob jobCtrl) repos

updateRepo :: UpdateFlags -> RepoContext -> Repo -> CabalM ()
updateRepo updateFlags repoCtxt repo = do
  transport <- repoContextGetTransport repoCtxt
  case repo of
    RepoLocal{..} -> return ()
    RepoRemote{..} -> do
      downloadResult <- downloadIndex transport repoRemote repoLocalDir
      case downloadResult of
        FileAlreadyInCache ->
          liftIO $ setModificationTime (indexBaseName repo <.> "tar") =<< getCurrentTime
        FileDownloaded indexPath -> do
          liftIO $ writeFileAtomic (dropExtension indexPath) . maybeDecompress
                                                  =<< BS.readFile indexPath
          updateRepoIndexCache (RepoIndex repoCtxt repo)
    RepoSecure{} -> repoContextWithSecureRepo repoCtxt repo $ \repoSecure -> do
      let index = RepoIndex repoCtxt repo
      -- NB: This may be a nullTimestamp if we've never updated before
      current_ts <- localVerbosity lessVerbose $ currentIndexTimestamp repoCtxt repo
      -- NB: always update the timestamp, even if we didn't actually
      -- download anything
      updated <- liftIO $ do
        writeIndexTimestamp index (fromFlag (updateIndexState updateFlags))
        ce <- if repoContextIgnoreExpiry repoCtxt
                then Just `fmap` getCurrentTime
                else return Nothing
        Sec.uncheckClientErrors $ Sec.checkForUpdates repoSecure ce
      -- Update cabal's internal index as well so that it's not out of sync
      -- (If all access to the cache goes through hackage-security this can go)
      case updated of
        Sec.NoUpdates  ->
          liftIO $ setModificationTime (indexBaseName repo <.> "tar") =<< getCurrentTime
        Sec.HasUpdates ->
          updateRepoIndexCache index
      -- TODO: This will print multiple times if there are multiple
      -- repositories: main problem is we don't have a way of updating
      -- a specific repo.  Once we implement that, update this.
      when (current_ts /= nullTimestamp) $
        noticeNoWrap $
          "To revert to previous state run:\n" ++
          "    cabal update --index-state='" ++ display current_ts ++ "'\n"
