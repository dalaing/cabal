{-# LANGUAGE CPP, PatternGuards #-}
-- This is a quick hack for uploading build reports to Hackage.

module Distribution.Client.BuildReports.Upload
    ( BuildLog
    , BuildReportId
    , uploadReports
    ) where

{-
import Network.Browser
         ( BrowserAction, request, setAllowRedirects )
import Network.HTTP
         ( Header(..), HeaderName(..)
         , Request(..), RequestMethod(..), Response(..) )
import Network.TCP (HandleStream)
-}
import Network.URI (URI, uriPath) --parseRelativeReference, relativeTo)

import Control.Monad
         ( forM_ )
import System.FilePath.Posix
         ( (</>) )
import qualified Distribution.Client.BuildReports.Anonymous as BuildReport
import Distribution.Client.BuildReports.Anonymous (BuildReport)
import Distribution.Text (display)
import Distribution.Verbosity (Verbosity)
import Distribution.Monad (CabalM)
import Distribution.Simple.Utils (die')
import Distribution.Client.HttpUtils
import Distribution.Client.Setup
         ( RepoContext(..) )

type BuildReportId = URI
type BuildLog = String

uploadReports :: RepoContext -> (String, String) -> URI -> [(BuildReport, Maybe BuildLog)] -> CabalM ()
uploadReports repoCtxt auth uri reports = do
  forM_ reports $ \(report, mbBuildLog) -> do
     buildId <- postBuildReport repoCtxt auth uri report
     case mbBuildLog of
       Just buildLog -> putBuildLog repoCtxt auth buildId buildLog
       Nothing       -> return ()

postBuildReport :: RepoContext -> (String, String) -> URI -> BuildReport -> CabalM BuildReportId
postBuildReport repoCtxt auth uri buildReport = do
  let fullURI = uri { uriPath = "/package" </> display (BuildReport.package buildReport) </> "reports" }
  transport <- repoContextGetTransport repoCtxt
  res <- postHttp transport fullURI (BuildReport.show buildReport) (Just auth)
  case res of
    (303, redir) -> return $ undefined redir --TODO parse redir
    _ -> die' "unrecognized response" -- give response

{-
  setAllowRedirects False
  (_, response) <- request Request {
    rqURI     = uri { uriPath = "/package" </> display (BuildReport.package buildReport) </> "reports" },
    rqMethod  = POST,
    rqHeaders = [Header HdrContentType   ("text/plain"),
                 Header HdrContentLength (show (length body)),
                 Header HdrAccept        ("text/plain")],
    rqBody    = body
  }
  case rspCode response of
    (3,0,3) | [Just buildId] <- [ do rel <- parseRelativeReference location
#if defined(VERSION_network_uri)
                                     return $ relativeTo rel uri
#elif defined(VERSION_network)
#if MIN_VERSION_network(2,4,0)
                                     return $ relativeTo rel uri
#else
                                     relativeTo rel uri
#endif
#endif
                                  | Header HdrLocation location <- rspHeaders response ]
              -> return $ buildId
    _         -> error "Unrecognised response from server."
  where body  = BuildReport.show buildReport
-}


-- TODO force this to be a PUT?

putBuildLog :: RepoContext -> (String, String)
            -> BuildReportId -> BuildLog
            -> CabalM ()
putBuildLog repoCtxt auth reportId buildLog = do
  let fullURI = reportId {uriPath = uriPath reportId </> "log"}
  transport <- repoContextGetTransport repoCtxt
  res <- postHttp transport fullURI buildLog (Just auth)
  case res of
    (200, _) -> return ()
    _ -> die' "unrecognized response" -- give response
