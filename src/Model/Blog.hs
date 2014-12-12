{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  RecordWildCards, TemplateHaskell, TypeFamilies,
  OverloadedStrings #-}

module Model.Blog (Blog, initialBlogState) where

import Model.BlogPost

import Prelude                 hiding (head)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Data
import Data.Acid
import Data.Acid.Advanced
import Data.IxSet           ( Indexable(..), IxSet(..), (@=)
                            , Proxy(..), getOne, ixFun, ixSet )
import qualified Data.IxSet as IxSet
import Data.SafeCopy
import Data.Time
import Data.Text            (Text, pack)
import Data.Text.Lazy hiding (empty)
import qualified Data.Text  as Text

data Blog = Blog
    { nextPostId :: Integer
    , posts      :: IxSet BlogPost
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Blog)

initialBlogState :: Blog
initialBlogState =
    Blog { nextPostId = 1
         , posts      = empty
         }

newPost :: UTCTime -> Update Blog BlogPost
newPost pubDate =
    do b@Blog{..} <- get
       let post = BlogPost 
       				{ postId 	= nextPostId
					, title 	= Text.empty
					, content 	= Text.empty
					, postedAt	= pubDate
					, tags   	= []
					}
       put $ b { nextPostId = succ nextPostId
               , posts      = IxSet.insert post posts
               }
       return post

updatePost :: BlogPost -> Update Blog ()
updatePost updatedPost = do
  b@Blog{..} <- get
  put $ b { posts =
             IxSet.updateIx (postId updatedPost) updatedPost posts
          }

postById :: PostId -> Query Blog (Maybe BlogPost)
postById pid =
     do Blog{..} <- ask
        return $ getOne $ posts @= pid

$(makeAcidic ''Blog
  [ 'newPost
  , 'updatePost
  , 'postById
  ])