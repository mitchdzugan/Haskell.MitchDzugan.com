{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  RecordWildCards, TemplateHaskell, TypeFamilies,
  OverloadedStrings #-}

module Model.Blog (Blog, initialBlogState) where

import Prelude hiding (head)
import qualified Data.Text as T

import Model.Import
import Model.BlogPost

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
					, title 	= T.empty
					, content 	= T.empty
					, postedAt	= pubDate
					, tags   	= []
					}
       put $ b { nextPostId = succ nextPostId
               , posts      = insert post posts
               }
       return post

updatePost :: BlogPost -> Update Blog ()
updatePost updatedPost = do
  b@Blog{..} <- get
  put $ b { posts =
             updateIx (postId updatedPost) updatedPost posts
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