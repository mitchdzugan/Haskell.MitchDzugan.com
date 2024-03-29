{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  RecordWildCards, TemplateHaskell, TypeFamilies,
  OverloadedStrings #-}

module Model.BlogPost (BlogPost(..), PostId,) where

import Model.Import

data BlogPost = BlogPost
    { postId 	:: Integer 
    , title 	:: Text
    , content 	:: Text
    , postedAt 	:: UTCTime
    , tags 		:: [Text]
    } deriving (Eq, Ord, Data, Typeable)

newtype PostId    = PostId Integer
    deriving (Eq, Ord, Data, Typeable)
newtype Title     = Title Text
    deriving (Eq, Ord, Data, Typeable)
newtype Tag       = Tag Text
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''BlogPost)
$(deriveSafeCopy 0 'base ''PostId)
$(deriveSafeCopy 0 'base ''Title)
$(deriveSafeCopy 0 'base ''Tag)

instance Indexable BlogPost where
    empty = ixSet
        [ ixFun $ \bp -> [ PostId $ postId bp ]
        , ixFun $ \bp -> [ Title $ title bp ]
        , ixFun $ \bp -> map Tag (tags bp)
        , ixFun $ \bp -> [ postedAt bp ]
        ]
