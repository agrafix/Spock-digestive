{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Web.Spock.Digestive
    ( runForm )
where

import Web.Spock.Core

#if MIN_VERSION_base(4, 8, 0)
#else
import Control.Applicative
#endif
import Control.Monad.Trans
import Network.HTTP.Types
import Network.Wai
import Text.Digestive.Form
import Text.Digestive.Types
import Text.Digestive.View
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

-- | Run a digestive functors form
runForm :: (Functor m, MonadIO m)
        => T.Text -- ^ form name
        -> Form v (ActionCtxT ctx m) a
        -> ActionCtxT ctx m (View v, Maybe a)
runForm formName form =
    do httpMethod <- requestMethod <$> request
       if httpMethod == methodGet
       then do f <- getForm formName form
               return (f, Nothing)
       else postForm formName form (const $ return localEnv)
    where
      localEnv path =
          do let name = fromPath path
                 applyParam f =
                     map (f . snd) . filter ((== name) . fst)
             vars <- applyParam TextInput <$> params
             sentFiles <- (applyParam (FileInput . uf_tempLocation) . HM.toList) <$> files
             return (vars ++ sentFiles)
