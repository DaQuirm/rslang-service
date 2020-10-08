{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Entity.WordSetWord where

import Prelude hiding (Word, id)
import GHC.Generics (Generic)
import Database.Selda (ID, SqlRow, Table, table, Attr((:-)), foreignKey)

import Entity.Word (Word, wordsTable)
import Entity.WordSet (WordSet, wordSetsTable)

data WordSetWord = WordSetWord
  { wordset :: ID WordSet
  , word    :: ID Word
  } deriving (Show, Generic)

instance SqlRow WordSetWord

wordSetWordsTable :: Table WordSetWord
wordSetWordsTable = table "wordset_words"
  [ #wordset :- foreignKey wordSetsTable #id
  , #word    :- foreignKey wordsTable #id
  ]
